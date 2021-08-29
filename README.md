# tagger

Tags files from filenames matching user-provided patterns. Uses `eyeD3` until I can write a native ID3 parser. Please install it through your package manager.

## Warning

Tag files at your own risk. There are still many ambiguities in pattern matching to sort out.

## Usage

```
tagger format-string files
```

where `format-string` is a string containing both plaintext and the following tag matchers

+ `{a}`: Artist name
+ `{b}`: Album artist name
+ `{A}`: Album title
+ `{G}`: Genre
+ `{t}`: Track title
+ `{d}`: Disc number
+ `{n}`: Track number
+ `{Y}`: Year
+ `{x}`: Matches text, then discards it

and `files` are the mp3 files to be tagged. Usually these are globbed as `*.mp3`, `**/*.mp3`, etc.

## Simple Example

Files

```
Tape1.jpg
Tape2.jpg
UPLOADED BY PAOLO COOLZONE.BR.RU 01.Monorail.mp3
UPLOADED BY PAOLO COOLZONE.BR.RU 02.Mässig.mp3
UPLOADED BY PAOLO COOLZONE.BR.RU 03.Future Rush Our.mp3
UPLOADED BY PAOLO COOLZONE.BR.RU 04.In The Deep Of Caverns.mp3
UPLOADED BY PAOLO COOLZONE.BR.RU 05.Uri.mp3
UPLOADED BY PAOLO COOLZONE.BR.RU 06.It's No Reason To Fear For.mp3
UPLOADED BY PAOLO COOLZONE.BR.RU 07.End Of The Jungle.mp3
cover1.jpg
cover2.jpg
```

Command

```
tagger "UPLOADED BY PAOLO COOLZONE.BR.RU {n}.{t}.mp3" *.mp3
```

The spam will be ignored, and the relevant tags extracted.

## Advanced example

The `{x}` matcher will discard any pattern it covers. This is useful for ignoring dynamic text, but can also serve as a shorthand for dropping literal strings.

For textual field matchers such as `{a}` or `{t}`, any text provided after the letter but within the matcher will be replaced by whitespace. `{a-}`, for example, will interpret `Nurse-with-Wound` as the artist name `Nurse with Wound`.

Files

```
Contemporary-Polish-Music_01_Krzysztof-Penderecki_To-the-Victims-of-Hiroshima.mp3
Contemporary-Polish-Music_02_Grazyna-Bacewicz_Music-for-Strings-Trumpets-and-Percussion.mp3
Contemporary-Polish-Music_03_Tadeusz-Baird_Erotica.mp3
Contemporary-Polish-Music_04_Kazimierz-Serocki_Sinfonietta-for-Two-String-Orchestras.mp3
```

Command

```
tagger "{A-}_{n}_{a-}_{t-}.mp3" *.mp3
```

The dashes are no problem. This pattern can be longer than one character, of course.

## Caveats

It is absolutely possible for the format string to be ambiguous. The command is only good if there are delimiters in the filename. stdout and stderr are suppressed because eyeD3 is ridiculously fucking noisy, but manually check the output; it may not be to your liking.
