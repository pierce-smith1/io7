// 
// io7.ts
// Windows 95/98 Boot Splash Decompressor and Compressor
// 
// # BACKGROUND
//
// In Windows 95 and 98, you can override the default boot splash by placing
// your own bitmap in the root of the C: drive with the name LOGO.SYS. When
// booting, Windows will display your bitmap instead of the default one.
//
// But where is the original bitmap actually stored? As Wikipedia will tell us, 
// it's kept in IO.SYS, a crucial DOS system file. But it's obscured with some 
// kind of bespoke properietary compression, so we cannot extract or replace it.
//
// Until now.
//
// This script implements a (very inefficient) compressor and decompressor for
// the custom compression format used for these graphics. While it can succesfully 
// extract and re-package logos for Windows 95 & 98, the main point of the 
// script is to serve as education and documentation for how this format works.
//
// The rest of this interlude will provide an exhaustive overview of what the
// data format looks like, including every single gory detail.
// Following this is the code itself, which provides a working 
// reference implementation for both compression and decompression.
//
// By default, this script does nothing when run - scroll to the very bottom and 
// uncomment one of the function calls to run the compressor or decompressor
// on the example files that have been included with this script.
// (from https://github.com/pierce-smith1/io7)
//
// # THE FORMAT
//
// In Windows 95's IO.SYS, the compressed logo data begins at offset 0x1b120 
// and ends at offset 0x2b17a. There are no other compressed regions in the
// file. It's the only example I know of of this exact compression format 
// being used. I'm going to call the format "IO7", after the IO from IO.SYS
// and the fact that 9x's DOS identifies itself as DOS 7.
//
// Fundamentally, IO7 is a variant on LZSS. Data is split up into
// tokens, coming one after another, that represent either literal bytes 
// or runs of bytes that have been previously seen. These tokens are grouped 
// into blocks that encode 8192 or fewer bytes of uncompressed data. Each
// block has a header that tells how big the compressed size is and whether 
// or not it's the last block. Each block in an IO7 "file" comes immediately
// after the last.
//
// We'll now go into each of these things in more detail.
//
// ## BLOCKS 
//
// An IO7 file is made entirely of "blocks" - it starts with a block, and blocks
// continue appearing one after another until the final block, after which the
// file simply ends.
//
// Blocks contain an 8-byte header, which crucially contains the size of the
// block, then an arbitrary number of bytes afterwards that contain the
// token stream encoding the compressed data. Unlike the token stream itself,
// blocks must be aligned to bytes, and in fact the end of the token stream
// will be padded with 0 bits to ensure the next block starts on a byte.
//
// Block headers are structured as follows:
//
// byte
// 0----1----2----3----4----5----6----7---
// size size ltag ltag 0x44 0x53 0x00 0x00
//
// The two size bytes are a 16-bit little-endian integer encoding the size 
// of the compressed data in this block. For whatever reason, the MSB is 
// always set to 1, meaning you must subtract 0x8000 to get the actual size.
// For example the two size bytes may be 0xBE 0x88, encoding an actual size of
// 0x88BE - 0x8000 = 0x08BE.
//
// The two ltag bytes are either [0x00 0x20] or [0x36 0x18]. They are [0x36 0x18]
// if and only if this is the last block in the file.
//
// After the ltag comes the fixed bytes [0x44 0x53 0x00 0x00] (DS..). Looking for
// this DS.. pattern can quickly identify the starts of compressed blocks in a 
// hex editor. For the purposes of the block size, this byte pattern is also 
// where the compressed data starts, i.e. the block size counts these 
// four bytes.
//
// ## TOKEN STREAMS
// 
// Immediately after a block header, the compressed data begins. The compressed
// data is a stream of tokens, each token coming one after another. Tokens are
// not aligned to bytes and often have sizes in bits that are not multiples
// of 8, so they must be read and written bit-by-bit.
//
// In the file, streams of bits are encoded by chopping the stream into bytes,
// where each byte contains 8 bits ordered from LSB to MSB. So for example, say
// we want to encode the following stream of bits (where letters a-p are bits):
//
// --------------->
// abcdefghijklmnop
// 
// They will appear in the bytes of the file as such:
//
// <------- <-------
// hgfedcba ponmlkji
// 0        1
//
// The bytes are read left to right, but the bits in those bytes are read
// right to left. Put another way, the "first" bit in a byte is in the
// least significant position.
//
// ### TOKENS 
//
// There are two main kinds of token: a literal token and an offset-length
// token. Each one represents one or more bytes of uncompressed data.
//
// #### LITERAL TOKENS
//
// A literal token encodes a single byte of uncompressed data. For notational 
// purposes, we can write these tokens as simply #hh, where hh are the two 
// hexadecimal digits of the encoded byte. For example, if we had a stream of 
// the following tokens:
//
// #00 #11 #22
//
// This encodes the uncompressed data [0x00 0x11 0x22].
//
// In the bitstream, literal tokens are 9 bits long. There is a single flag bit 
// followed immediately by the 8 bits of the byte. A literal token looks as 
// follows in a stream:
//
//    --------->
// ...f Dddddddd...
//    ^ ^
//    | |
//    ' flag bit
//      |
//      ' data bits - D is the MSB
//
// The flag bit is the inverse of the MSB of the data. If D is 0, then f is 1 -
// if D is 1, then f is 0. For example, we would encode the byte 0xB5 in a bit
// stream as follows:
//
//    --------->
// ...0 10110101...
//    ^ ^
//    | |
//    ' flag bit, is 0 because the MSB of the data is 1
//      |
//      ' the byte (0xB5) as binary
// 
// #### OFFSET-LENGTH TOKENS
//
// Offset-length tokens encode a sequence of at least 2 bytes through two
// numbers: an _offset_ into the uncompressed data, and a _length_ of how many 
// bytes to read from this offset.
//
// For notational purposes, we can write these tokens as <O,L>, where O is the
// offset and L is the length. O must be >= 1, and L must be >= 2.
//
// The offset is backwards relative to our current position in the uncompressed
// stream of bytes, and the length is the count of bytes to read. For example,
// say we have the following token stream:
//
// #0a #1b #2c #3d <3,2>
//
// The first four tokens represent the uncompressed data [0x0a 0x1b 0x2c 0x3d]. 
// Now, when the token <3,2> is encountered, this tells us to look back 3 bytes 
// from the end of our uncompressed data, then read 2 bytes forward.
//
// [0x0a 0x1b 0x2c 0x3d]
//                      ^
//                      |
//                      ' start past the end
//
// [0x0a (0x1b 0x2c 0x3d]
//       ^
//       |
//       ' move 3 bytes backwards...
//
// [0x0a (0x1b 0x2c) 0x3d]
//                 ^
//                 |
//                 ' ...then 2 bytes forwards
// 
//       (0x1b 0x2c) is what <3,2> encodes
//
// So, in this stream, <3,2> encodes [0x1b 0x2c], and our final uncompressed data
// is the sequence [0x0a 0x1b 0x2c 0x3d 0x1b 0x2c].
//
// It is valid for the length to be greater than the offset. In this case,
// bytes read off the end of the uncompressed data repeat the data read from
// from the offset to the end of the data. For example, take this stream of tokens:
//
// #00 #11 #22 #33 <3,7>
//
// To read the <3,7> token:
//
// [0x00 0x11 0x22 0x33]
//                      ^
//                      |
//                      ' start past the end
//
// [0x00 (0x11 0x22 0x33)]
//       ^
//       |
//       ' 3 bytes backwards...
//
// [0x00 (0x11 0x22 0x33 ?? ...
//                       ^
//                       |
//                       ' ...then 7 bytes forwards -
//                            but uh oh, we're out of bytes to read!
//
// [0x00 (0x11 0x22 0x33 0x11 ...
//       ^               ^   
//       '---------------|
//                       ' ...so we pretend the bytes repeat from
//                            where we started reading!
//
// [0x00 (0x11 0x22 0x33 0x11 0x22 ...
//             ^              ^   
//             '--------------|
//                            ' continue...
//
// [0x00 (0x11 0x22 0x33 0x11 0x22 0x33 0x11)]
//                       ^              ^   
//                       '--------------|
//                                      ' ...until we've made enough bytes
//
//       (0x11 0x22 0x33 0x11 0x22 0x33 0x11) is what <3,7> encodes
// 
// It is not valid for the offset to be greater than the length of the
// uncompressed data.
//
// In the bitstream, offset-length tokens are a variable number of bits. The
// first group is bits encodes the offset and the second group encodes the
// length, both of which are variable size.
//
// Consider the offset first. Depending on the offset's size, we use three
// different encoding strategies.
//
// * If the offset is representable in 6 bits, i.e. if it is less than 64, 
//   it is a "short" offset, and will be encoded in 8 bits. Start with two 0 bits, 
//   then write the 6 bits of the offset. Example: An offset of 21 (0x15) will
//   appear as such:
//
//      -------->
//   ...00 010101...
//      ^  ^ 
//      |  |
//      ' short offset flag
//         |
//         ' 21 (0x15) in binary
// 
// * If the offset - 64 is repesentable in 8 bits, i.e. if it is >= 64 and < 320, 
//   it is a "medium" offset, and will be encoded in 13 bits. Start with two 1 bits, 
//   then a 0 bit, then write the 8 bits of the offset - 64. Example: An offset of 
//   152 (0x98) will appear as such:
//
//   ...110 01011000...
//      ^   ^
//      |   |
//      ' medium offset flag
//          |
//          ' 152 - 64 = 88 (0x58) in binary
//
// * If the offset - 319 is representable in 12 bits i.e. if it is >= 320 and < 4415,
//   it is a "far" offset, and will be encoded in 15 bits. Start with three 1 bits, 
//   then write the 12 bits of the offset - 320. Example: an offset of 2270 will 
//   appear as such:
//
//   ...111 011110011110...
//      ^   ^
//      |   |
//      ' far offset flag
//          |
//          ' 2270 - 320 = 1950 (0x79e) in binary
//
//   Note that the offset cannot be 4415, even though 4415 - 320 = 4095 can be
//   represented in 12 bits as a sequence of all 1s. This would result in a 
//   sequence of 15 consecutive 1s, which is interpreted as a special token
//   we will talk about later.
//
//   Note also that this defines 4414 as the largest representable offset.
//
// Immediately following the offset is the length. Again, the length is encoded
// differently based on how large it is - however, this encoding scheme is more
// mathematical than case-based.
//
// We split the length into two numbers, a "base" and a "tail". The base is the
// largest number that is a power of 2 plus 1 which is still smaller than the
// length, and the tail is simply the length minus the base. For example, if 
// the length is 13, the largest number n^2 + 1 that fits within it is 9 (2^3 + 1), 
// so the base is 9 and the tail is 13 - 9 = 4. Another example, if the length 
// is 40, the base is 33 (2^5 + 1) and the tail is 40 - 33 = 7.
//
// To represent the length in the bitstream, first encode the base by writing 
// n 0s, where n is the power of the base. Then write a 1, then finish by
// writing the tail in n bits.
//
// Let's use 13 as an example again. The base is 2^3 + 1, so we write 3
// 0 bits, then a 1 bit. Finally we write the tail, 4, in 3 bits.
//
// ...000 1 100...
//    ^   ^ ^
//    |   | |
//    n 0s, where n = the power of the base (in this case, 3)
//        | |
//        ' fixed 1 following the 0s
//          |
//          ' the tail (4) in 3 bits
//
// Another example: to encode a length of 40, the base is 2^5 + 1, so we
// write 5 0 bits, then a 1 bit, and finally the tail of 7 in 5 bits.
//
// ...00000 1 00111...
//    ^     ^ ^
//    |     | |
//    n 0s, where n = the power of the base (in this case, 5)
//          | |
//          ' fixed 1 following the 0s
//            |
//            ' the tail (7) in 5 bits
//
// Let's put everything together into a final example.
//
// Say we want to encode the token <343,10>. The offset is 343 and the length
// is 10. We start by encoding the offset. 
//
// The offset is larger than 320 and smaller than 4415, so it is a "far" 
// offset, and it is encoded as follows:
//
//  ..111 000000010111...
//    ^   ^
//    |   |
//    ' far offset flag
//        |
//        ' 343 - 320 = 27 (0x17) in binary in 12 bits
//
// Now immediately afterwards we encode the length. The length is 10, so we
// have a base of 9 (2^3 + 1). The tail is 10 - 9 = 1. This adds to our encoding
// as follows:
//
//  ...111000000010111 000 1 001...
//     ^               ^   ^ ^
//     |               |   | |
//     ' the offset    ' three 0s (since the base is 2^3 + 1)
//                         | |
//                         ' fixed 1 following the 0s
//                           |
//                           ' the tail (1) in three bits
//
// And the final token is:
//
//  ...1110000000101110001001...
//
// #### SENTINEL TOKENS
//
// There is a final type of token that encodes no data. It MUST appear as the
// final token of a block, and it also MUST appear after 512 bytes of
// uncompressed data have been encoded. In other words, every sequence of
// tokens between two sentinel tokens MUST encode 512 or fewer bytes of data.
// This means is also invalid for a single token to encode more than 512 bytes.
//
// Its bitstream representation is simply 15 consecutive 1s. 
//
// That's all! That's the compression format in its entirety.
// We now know enough to build a working compressor and decompressor.
//
// The rest of this file will contain a reference implementation for a
// decompressor and compressor, using what we've detailed above. But first, I 
// want to quickly make an acknowledgement:
//
// # ACKNOWLEDGEMENTS
// 
// This work is indebted to a programmer simply credited "bean", who wrote a 
// decompressor for the Windows ME IO.SYS format for an open-source project 
// called grub4dos.
//
// (https://github.com/chenall/grub4dos/blob/3c1d05f39e49ec1d7543caa825df00068b96620b/stage2/builtins.c#L441-L621)
//
// Their code was instrumental in guiding me through the reverse-engineering
// process. It it wasn't for them, I would never have made it even close to 
// understanding the IO7 format and being able to write these algorithms.
//
// Thank you, bean.
//

//
// # ALGORITHMS
//
// What follows now is some actual code for implementing compression and
// decompression in the IO7 format.
//

// We define a convenience class that wraps an array of bytes and lets us
// read and write from it as if it were a stream of bits. 
class BitStream {
    constructor (
        // Throughout this program, we will use the `number` type to implicitly
        // mean a byte, and just make the assumption it always stays in the range
        // 0 - 255.
        private bytes: Array<number>,

        // bit_pos represents our position in the current byte - as such it
        // stays between 0 and 7. 
        // 0 means we're at the LSB of the byte, and 7 the MSB.
        private bit_pos = 0,

        // byte_pos is effectively an index into the byte array.
        // As we travel bit-by-bit through the stream, byte_pos will increase
        // slowly without bound, and bit_pos will cycle between 0 and 7.
        private byte_pos = 0,
    ) {}

    get_bytes() {
        return [...this.bytes];
    }

    get_bit_offset() {
        return this.bit_pos + (this.byte_pos * 8);
    }

    advance_bit_pos() {
        this.bit_pos++;

        if (this.bit_pos >= 8) {
            this.bit_pos = 0;
            this.byte_pos++;
        }
    }

    advance_bit_pos_by(num_bits: number) {
        for (let i = 0; i < num_bits; i++) {
            this.advance_bit_pos();
        }
    }

    reverse_bit_pos() {
        this.bit_pos--;

        if (this.bit_pos < 0) {
            this.bit_pos = 7;
            this.byte_pos--;
        }
    }

    reverse_bit_pos_by(num_bits: number) {
        for (let i = 0; i < num_bits; i++) {
            this.reverse_bit_pos();
        }
    }

    // This returns the special sentinel "eof" if we run out of bits.
    read_bits(num_bits: number): number | "eof"  {
        const end_bit = (this.bit_pos + num_bits) % 8;
        const end_byte = this.byte_pos + Math.floor((this.bit_pos + num_bits) / 8);

        let result = 0;
        let bits_read = 0;
        while (this.bit_pos !== end_bit || this.byte_pos !== end_byte) {
            const byte = this.bytes[this.byte_pos];

            if (byte === undefined) {
                return "eof";
            }
            
            const next_bit = (byte & (1 << this.bit_pos)) ? 1 : 0;
            result += next_bit << bits_read++;
            this.advance_bit_pos();
        }

        return result;
    }

    write_bits(n: number, num_bits: number) {
        let bits_written = 0;
        while (bits_written < num_bits) {
            this.write_bit((n >>> bits_written & 1) as 0 | 1);
            bits_written++;
        }
    }

    write_bit(bit: 0 | 1) {
        if (this.byte_pos >= this.bytes.length) {
            this.bytes.push(0);
        }

        this.bytes[this.byte_pos] += bit << this.bit_pos;

        this.advance_bit_pos();
    }

    is_at_end(): boolean {
        return this.bytes[this.byte_pos] === undefined;
    }
}

// A model of our tokens will prove very useful.
// They are perfectly described by a tagged union.
type Token = 
    | {type: "literal", value: number}
    | {type: "offset", offset: number, length: number}
    | {type: "sentinel"}

//
// ## DECOMPRESSION ROUTINES
//

// This function will read a token from a stream of bits, advancing the stream
// to the start of the next token.
// It will return undefined when there are no more valid tokens to read.
// The idea is that you can call this repeatedly on the same bitstream until it 
// returns undefined to enumerate all the tokens in the stream.
function read_token(bits: BitStream): Token | undefined {
    // To avoid having to constantly check for "eof" after reading from the
    // stream, we define this helper that throws an exception on eof. Then in 
    // our logic, we trap the exception to immediately return undefined.
    function read_bits_or_die(num_bits: number): number {
        const result = bits.read_bits(num_bits);
        if (result === "eof") {
            throw new Error("Out of bits");
        }

        return result;
    }

    try {
        // We need to read at least two bits to start figuring out what kind of
        // token we're looking at.
        const tag = read_bits_or_die(2);

        // We can tell if this is a literal byte by if the two bits of the tag
        // are different.
        if (tag === 1 || tag === 2) {
            const literal_byte_end = read_bits_or_die(7);

            // This reconstructs the original byte from part of the tag, since
            // they overlap.
            const literal_byte = literal_byte_end + ((tag & 1) << 7);

            return {type: "literal", value: literal_byte};
        }

        // If we get here, we must be reading either a sentinel or an 
        // offset-length token.
        // A sentinel resembles a far offset-length token enough that we can
        // check for it in the process of parsing an offset-length token, so at
        // this point we just assume we're looking at an offset-length token.
        const offset = (() => {
            if (tag === 0) {
                const short_offset = read_bits_or_die(6);
                return short_offset;
            }

            const is_far_offset = bits.read_bits(1);
            if (is_far_offset === 1) {
                const far_offset = read_bits_or_die(12);
                return far_offset + 0x140;
            }

            const med_offset = read_bits_or_die(8);
            return med_offset + 0x40;
        })();

        // 0x113F - 0x140 === 0xFFF. So with the three 1s of the tag included,
        // if we see this exact offset, we've seen fifteen 1s in a row.
        if (offset === 0x113F) {
            return {type: "sentinel"};
        }

        const length = (() => {
            let bits_in_length = 0;
            while (read_bits_or_die(1) === 0) {
                bits_in_length++;
            }

            const base = (1 << bits_in_length) + 1;
            const tail = read_bits_or_die(bits_in_length);
            
            const length = base + tail;
            return length;
        })();

        return {type: "offset", offset, length};
    } catch {
        // Using exceptions for control flow - very naughty indeed!
        return undefined;
    }
}

// This function unpacks a stream of tokens into the uncompressed bytes 
// they represent.
function decode_tokens(tokens: Array<Token>): Array<number> {
    // We need to keep track of the bytes we've uncompressed so far so that we
    // can decompress offset-length tokens.
    let output: Array<number> = [];

    function segment_to_bytes(segment: Token): Array<number> {
        switch (segment.type) {
            case "literal": return [segment.value];
            case "offset": {
                const end_index = -segment.offset + segment.length;
                let new_bytes = [...output.slice(-segment.offset, end_index >= 0 ? undefined : end_index)];

                // If the length was greater than the offset, we tried to read
                // past the end of the array.
                // We need to fill the missing pieces with the last byte of
                // the output.
                while (new_bytes.length !== segment.length) {
                    new_bytes.push(new_bytes.at(-segment.offset));
                }

                return new_bytes;
            }
            case "sentinel": return [];
        }
    }

    for (const segment of tokens) {
        const bytes = segment_to_bytes(segment);
        output = [...output, ...bytes];
    }

    return output;
}

// This separates a bitstream into blocks of raw compressed data by searching
// for, splitting on, and stripping out DS headers.
// The result is an array of bitstreams that contain raw data from which we
// can read tokens.
function break_into_blocks(bits: BitStream): Array<BitStream> {
    let blocks = [];

    let block_size = bits.read_bits(16);
    while (!bits.is_at_end() && block_size !== "eof" && block_size >= 0x8000) {
        // Skip the rest of the DS header by moving forwards 6 bytes - 
        // A DS header is 8 bytes but we already read 2 of them.
        bits.advance_bit_pos_by(6 * 8);
        
        const this_block = [];
        for (let i = 0; i < block_size - 0x8000 - 4; i++) {
            // We can just assume these reads will never be eofs, since if they
            // are, the header lied to us about big the block is..!
            const byte = bits.read_bits(8) as number;        
            this_block.push(byte);
        }

        blocks.push(this_block);

        block_size = bits.read_bits(16);
    }

    return blocks.map(block => new BitStream(block));
}

// Putting everyting together, this function turns a bitstream holding a
// complete IO7 file into a stream of uncompressed bytes!
function decompress(compressed_bits: BitStream): Array<number> {
    const blocks = break_into_blocks(compressed_bits);

    const output_bytes = blocks.flatMap(block => {
        let tokens = [];

        let next_token: Token | undefined;
        while ((next_token = read_token(block))) {
            tokens.push(next_token);
        }

        const output = decode_tokens(tokens);
        return output;
    });

    return output_bytes;
}

//
// ## COMPRESSION ROUTINES
//

// This function turns a token into its bit representation and writes it to
// a bitstream.
function write_token(segment: Token, bits: BitStream) {
    if (segment.type === "literal") {
        const byte = segment.value;

        const high_bit = byte & (1 << 7) ? 1 : 0;

        bits.write_bit(high_bit);
        bits.write_bit(high_bit === 0 ? 1 : 0);
        bits.write_bits(byte, 7);
    } else if (segment.type === "offset") {
        const {offset, length} = segment;

        const offset_size = (() => {
            if (offset < 0x40) {
                return "short";
            } else if (offset < 0x140) {
                return "medium";
            } else {
                return "far";
            }
        })();

        if (offset_size === "short") {
            bits.write_bits(0, 2);
            bits.write_bits(offset, 6);
        } else {
            bits.write_bits(3, 2);
        }

        if (offset_size === "medium") {
            bits.write_bit(0);
            bits.write_bits(offset - 0x40, 8);
        }

        if (offset_size === "far") {
            bits.write_bit(1);
            bits.write_bits(offset - 0x140, 12);
        }

        let bits_in_length = (length => {
            let num_bits = 0;
            while (length >>> num_bits) {
                num_bits++;
            }
            return num_bits;
        })(length - 1);

        for (let i = 0; i < bits_in_length - 1; i++) {
            bits.write_bit(0);
        }

        bits.write_bit(1);

        bits.write_bits(length - 1 - (1 << bits_in_length), bits_in_length - 1);
    } else {
        bits.write_bits(0xFFFF, 15);
    }
}

// This function turns a stream of uncompressed bytes into a stream of tokens 
// that encode the compressed data.
// This is where the compression magic really happens.
function encode_bytes(bytes: Array<number>): Array<Token> {
    const bits = new BitStream(bytes);

    // We need to keep track of where we are in the uncompressed data so we can
    // correctly count offsets for offset-length tokens.
    let input_cursor = 0;
    const output_segments: Array<Token> = [];

    function commit_segment(segment: Token, from_bytes: Array<number>) {
        input_cursor += from_bytes.length;
        output_segments.push(segment);
    }

    function array_equal<T>(a1: Array<T>, a2: Array<T>): boolean {
        return a1.length === a2.length && a1.every((_, i) => a1[i] === a2[i]);
    }

    // A "phrase" is some arbitray run of bytes. This helper searches through
    // the input we have currently processed to see if that phrase has
    // already appeared.
    // It's used to tell if we can use an offset-length token to encode the
    // given phrase.
    // It returns the index the phrase begins in our input bytes, or undefined
    // if it can't be found.
    function find_phrase_in_input(phrase: Array<number>): number | undefined {
        const seen_input = bytes.slice(0, input_cursor);

        // Using findLastIndex here helps keep the offset small, since offsets
        // are negative relative to the end.
        const phrase_index = seen_input.findLastIndex((byte, i, bytes) => {
            let searched_bytes = bytes.slice(i, i + phrase.length);

            if (searched_bytes.length < phrase.length) {
                searched_bytes = [...searched_bytes, ...new Array(phrase.length - searched_bytes.length).fill(seen_input.at(-1))];
            }

            return array_equal(phrase, searched_bytes);
        });

        return phrase_index !== -1 ? phrase_index : undefined;
    }

    // The main compression loop, running until we're out of input.
    while (!bits.is_at_end()) {
        // We want to opportunistically read two bits at a time so we can
        // start trying to build a phrase that could be encoded in an 
        // offset-length token. 
        // If we only ever read one byte at a time, we can never make a phrase,
        // since they have to be at least two bytes.
        let maybe_phrase = [bits.read_bits(8), bits.read_bits(8)];

        if (maybe_phrase[0] === "eof") {
            break;
        }

        if (maybe_phrase[1] === "eof") {
            commit_segment({type: "literal", value: maybe_phrase[0]}, [maybe_phrase[0]]);
            break;
        }

        let phrase = maybe_phrase as Array<number>;

        let last_input_phrase_index: number | undefined;
        let input_phrase_index: number | undefined;

        // See if our phrase is in the input, and if it is, greedily keep 
        // trying to grow it as much as possible.
        while ((input_phrase_index = find_phrase_in_input(phrase)) !== undefined) {
            const next_byte = bits.read_bits(8);

            // The -1 here is an ugly hack to handle the special case of our 
            // phrase getting cut off by the end of the input.
            phrase = [...phrase, next_byte === "eof" ? -1 : next_byte];
            last_input_phrase_index = input_phrase_index;
        }

        if (phrase.at(-1) !== -1) {
            // Once the above while loop exits, we will have read one byte that
            // didn't fit into our phrase - so we need to back up so that we can
            // consider that byte again in the next loop around.
            // The if condition guards against the case that the phrase was cut
            // off by the end of the stream, in which case we DON'T want to
            // reverse the stream since nothing was read.
            bits.reverse_bit_pos_by(8);
        }

        // The last byte we read was NOT part of this phrase.
        phrase = phrase.slice(0, -1);

        if (phrase.length === 1) {
            commit_segment({type: "literal", value: phrase[0]}, [phrase[0]]);
            continue;
        }

        commit_segment({type: "offset", offset: input_cursor - last_input_phrase_index, length: phrase.length}, phrase);
    }

    commit_segment({type: "sentinel"}, []);
    return output_segments;
}

// This function reads up to 8192 bits of uncompressed data (enough to make a 
// block) from the input stream, and outputs a stream of tokens to store in
// the block.
// It returns undefined when the stream is over and no more blocks can be made.
function encode_next_block(bits: BitStream): Array<Token> | undefined {
    if (bits.is_at_end()) {
        return undefined;
    }

    // There must be a sentinel every 512 bytes -
    // We call a stretch of 512 bytes a "field".
    const max_bytes_in_field = 512;
    const max_fields_in_block = 16;
    const max_bytes_in_block = max_bytes_in_field * max_fields_in_block; // 8192!

    const block_bytes = (() => {
        let bytes: Array<number> = [];

        for (let i = 0; i < max_bytes_in_block; i++) {
            const next_byte = bits.read_bits(8);
            if (next_byte === "eof") {
                break;
            }
            bytes.push(next_byte);
        }

        return bytes;
    })();

    const fields = block_bytes.flatMap((byte, i, bytes) => {
        return i % max_bytes_in_field
            ? []
            : [bytes.slice(i, i + max_bytes_in_field)];
    });

    const segments = fields.flatMap(encode_bytes);
    return segments;
}

// Putting everyting together, this function turns a bitstream holding
// arbitrary data into a stream of bytes holding a complete compressed 
// IO7 file!
function compress(input_bits: BitStream): Array<number> {
    let blocks: Array<Array<Token>> = [];

    let next_block: Array<Token> | undefined;
    while ((next_block = encode_next_block(input_bits))) {
        blocks.push(next_block);
    }

    const encoded_blocks = blocks.map(tokens => {
        const bits = new BitStream([]);
        tokens.forEach(token => write_token(token, bits));
        return bits;
    }).map(bits => bits.get_bytes());

    const output_bytes = [];
    encoded_blocks.forEach((encoded_block, i, blocks) => {
        // If this is the last block, use the special last block tag
        const ds_header_tag = i !== blocks.length - 1 
            ? [0x00, 0x20]
            : [0x36, 0x18];
        
        const ds_header = [...ds_header_tag, 0x44, 0x53, 0x00, 0x00];
        
        const block_size = encoded_block.length + 0x8004;
        const size_header = [block_size & 0xff, ((block_size & 0xff00) >> 8)];
    
        output_bytes.push(...size_header, ...ds_header, ...encoded_block);
    });

    return output_bytes;
}

// Finally, some helpers to facilitate compressing and decompressing to and 
// from files.
import * as Fs from "fs";

function decompress_file(input_filename: string, output_filename: string) {
    const input_bits = new BitStream([...new Uint8Array(Fs.readFileSync(input_filename))]);

    const decompressed_bytes = decompress(input_bits);

    Fs.writeFileSync(output_filename, new Uint8Array(decompressed_bytes));
}

function compress_file(input_filename: string, output_filename: string) {
    const input_bits = new BitStream([...new Uint8Array(Fs.readFileSync(input_filename))]);

    const compressed_bytes = compress(input_bits);

    Fs.writeFileSync(output_filename, new Uint8Array(compressed_bytes));
}

// Try it!

//decompress_file("logo.io7", "logo.bmp");
//decompress_file("logo98.io7", "logo98.bmp");
//compress_file("ftlogo.bmp", "ftlogo.io7");

// And that's all, folks.
// PS <3
