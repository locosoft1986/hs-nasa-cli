<p align="center">
  <img src="https://i.imgur.com/GNPXJQC.png" href="https://www.nasa.gov/" height="256">
  <h2 align="center">HASKELL NASA CLI</h2>
  <p align="center">🚀 Download NASA Picture of the Day from your terminal written in Haskell!<p>

<p align="center"><img src="https://i.imgur.com/TGG4tXh.gif" alt="Picture of the Day!"></p>

## Installation

* Clone the repo
* `stack install`
* Ensure your `$PATH` includes the default Stack directory
* Run `hs-nasa-cli` with options


For Mac x64 you can download hs-nasa-cli executable from "releases/mac_x64" directory.

## Usage

``` bash
# Download Picture of the Day

$ hs-nasa-cli -t

# Download Picture of the Day from the specific date

$ hs-nasa-cli -d YYMMDD

# Examples:

$ hs-nasa-cli -t

$ hs-nasa-cli -d 171224
```

## How it works?

It downloads the latest Picture of the Day (or from specific date) from [NASA APOD](https://apod.nasa.gov/apod/) and saves it on your computer (in the directory, where terminal is executed :unicorn:).



## Thanks:
- [Original Node Js Nasa CLI](https://github.com/xxczaki/nasa-cli) for having a project to be practice with Haskell.
- [Pedro Yamada](https://github.com/yamadapc/haskell-questioner) for terminal spinner, I have to modify a little bit to support colors for spinner animation.
- [NASA APOD :rocket:](https://apod.nasa.gov/apod/) for providing marvelous space photos every day!

## Disclaimer

NASA CLI is not affiliated with National Aeronautics and Space Administration.

## License

MIT

## TODO
[ ] Support Youtube video downloading.
[ ] Cleaner code in Main function.
