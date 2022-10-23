# Recreating ST-NICC 2000 1st place demo in F#

ST-NICC aka "ST News International Christmas Coding Convention" had a demo competition in 2000, the [winner](https://www.youtube.com/watch?v=nqVJWFNpTqA) was [Oxygene](https://demozoo.org/groups/2118/) with an amazing 3D screen for Atari STE.

I did play a small part in providing the PRO tracker player.

I much later found out that there was competition in 2019 to port the Oxygene demo on different platforms.

There were some amazing ones like:

* [Amiga](https://www.youtube.com/watch?v=7sYhIxiizKY)
* [Atari STF](https://www.youtube.com/watch?v=8VOCbmMMteY&t=3s)
* [SNES](https://www.youtube.com/watch?v=dOqxLBZiBRA)
* [Atari 800XL](https://www.youtube.com/watch?v=rVa0ZEdwqoY)
* [BBC Micro](https://www.youtube.com/watch?v=_mVI9d2Acyw)

## F# advent 2022

I sometimes write for F# advent, usually about things that have no use to anyone but perhaps interesting to a few people out there.

I was thinking, could I recreate the ST-NICC winner in F# for F# advent?

From the invite to port the demo there was a format description as well as the original data file.

So in theory yes.

## Parsing a 20 year old binary format

Because CPU was a bit more limited on the old Atari STE than today [Leonard](https://demozoo.org/sceners/2527/) had prerendered all the coordinate transforms for the entire 3D scene and stored them in a compact binary format, around 600KiB. As the most common Atari STE had 1024KiB of memory that didn't leave much memory for other things like my memory hungry PRO Tracker player.

So [Leonard](https://demozoo.org/sceners/2527/) streamed the data from disk into memory on-demand while the demo was running.

In order to recreate the ST-NICC winner we need to parse the binary format

### The format

See the [FORMAT.md](FORMAT.md) for all the details but:

1. The entire scene is composed of 1,800 frames.
2. A frame has a flag to indicate if the screen needs clearing, a palette delta and the polygons
3. A polygon is either indexed (to save space) or non-indexed
4. If non-indexed polygons has palette index and vertices
5. If indexed polygons starts with all vertices then palette index and indexes to the vertices

This can in F# be represented like this:

```fsharp
// Atari STE used RGB colors but only had 4096
//  Only the low nibble in each byte used
type [<Struct>] RGB         =
  {
    Red     : byte
    Green   : byte
    Blue    : byte
  }
// The vertices were 2 bytes, meaning the 3D
//  model didn't utilize the full 320x200 resolution of the screen
//  but was limited to 256x200
type [<Struct>] Vertex2D    =
  {
    X   : byte
    Y   : byte
  }
// The index of the color in the palette
type [<Struct>] ColorIndex  =
  {
    Index   : byte
  }
// The palette of Atari STE was 16 colors
//  The first (0) being the background color
type [<Struct>] PaletteItem =
  {
    ColorIndex  : ColorIndex
    Color       : RGB
  }
// The polygon has an index to the color to use
//  and the vertices
//  While the model has an indexed mode as well
//  when parsing I converted to non-indexed as
//  a Win10 machine don't struggle with a few extra KiBs.
type [<Struct>] Polygon     =
  {
    ColorIndex  : ColorIndex
    Vertices    : Vertex2D array
  }
// The frame
//  ClearScreen is used to tell if we should clear screen
//    between frames. Clearing screen takes time so why do it if
//    it's not needed?
//  PaletteDelta is an array of 0..16 elements
//    It's used to update the current palette of 16 colors
//    Just overwrite the color at the index of the palette item
//  Polygons is an array of polygons for the frame
type [<Struct>] Frame =
  {
    ClearScreen   : bool
    PaletteDelta  : PaletteItem array
    Polygons      : Polygon array
  }
// The entire scene is just all frames
type [<Struct>] Scene =
  {
    Frames        : Frame array
  }
```

Once parsed we "just" render frame by frame to recreate the ST-NICC winner.

### The parser

I thought a binary parser combinator makes sense, like so:

```fsharp
type 'T BinaryReader = byte array -> int -> struct ('T*int)
```

So a `'T BinaryReader` takes a byte array and an int and product a value `'T` and the position of after the value in the byte array.

When parsing text files it's common to return an option or something similar as it's common in text parsers to attempt different parsers to find that one that matches. Binary parsers tend to be more read a value that tells you what the following bytes represent, pick the correct parser and parse the bytes. If the input is incorrect we just throw and stop the entire parsing process.

The format consists of either byte or word (2 bytes) values so we need parsers for thus:

```fsharp
  let inline bbyte () : uint8 BinaryReader = fun bs i ->
    struct (bs.[i], i + 1)

  let inline bword () : uint16 BinaryReader = fun bs i ->
    let word =
        ((uint16 bs.[i]) <<< 8)
      + (uint16 bs.[i +  1])
    struct (word, i + 2)
```

To this I added the computation expression builder:

```fsharp
  type BinaryReaderBuilder () =
    class
      member inline x.Bind  ( [<InlineIfLambda>] t  : _ BinaryReader
                            , [<InlineIfLambda>] uf : _ -> _ BinaryReader
                            ) : _ BinaryReader = fun bs i ->
        let struct (tv, ti) = t bs i
        uf tv bs ti

      member inline x.Combine ( [<InlineIfLambda>] t : _ BinaryReader
                              , [<InlineIfLambda>] u : _ BinaryReader
                              ) : _ BinaryReader =  fun bs i ->
        let struct (_, ti) = t bs i
        u bs ti

      member inline x.MergeSources  ( [<InlineIfLambda>] t : _ BinaryReader
                                    , [<InlineIfLambda>] u : _ BinaryReader
                                    ) : _ BinaryReader =  fun bs i ->
        let struct (tv, ti) = t bs i
        let struct (uv, ui) = u bs ti
        struct (struct (tv, uv), ui)

      member inline x.BindReturn    ( [<InlineIfLambda>] t : _ BinaryReader
                                    , [<InlineIfLambda>] f
                                    ) : _ BinaryReader =  fun bs i ->
        let struct (tv, ti) = t bs i
        struct (f tv, ti)

      member inline x.Return  ( v : 'T
                              ) : 'T BinaryReader =
        bvalue v
      member inline x.ReturnFrom  (br : 'T BinaryReader
                                  ): 'T BinaryReader =
        br
    end
  let breader = BinaryReaderBuilder()
```

My hope by making the functions inline and `[<InlineIfLambda>]` is to reduce overhead from the parser combinator. The problem I found was that it didn't really help for `Bind` that much but at least it improved it in those cases where applicatives could be used (`MergeSources` and `BindReturn`).

This allows us to define the basic building blocks:

