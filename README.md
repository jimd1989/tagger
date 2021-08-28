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

and `files` are the mp3 files to be tagged. Usually these are globbed as `*.mp3`, `**/*.mp3`, etc.

## Example

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

## Caveats

It is absolutely possible for the format string to be ambiguous. The command is only good if there are delimiters in the filename. stdout and stderr are suppressed because eyeD3 is ridiculously fucking noisy, but manually check the output; it may not be to your liking.
