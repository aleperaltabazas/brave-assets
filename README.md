# brave-assets

[Scraper](scraper) was made with Haskell, using [Deathmax's brave info](https://github.com/cheahjs/bravefrontier_data/) as a base for the units list, and with Scalpel to scrape the [Brave Frontier Global wiki](https://bravefrontierglobal.fandom.com/wiki/Brave_Frontier_Wiki)

Generated assets can be found in its [correspondent folder](assets).

Files are being hosted at [Cloudinary](https://cloudinary.com/) ([here](https://res.cloudinary.com/proyectate/image/upload/v1626835888/brave-assets/zephu_sgdo2c.png)'s an exampe).

## Installation

If you wish to dump Brave Frontier's unit arts, clone this repo and then run `stack install` (if you don't have Stack, you can follow [these instructions](https://docs.haskellstack.org/en/stable/README/). This will give you access to the `brave-assets` program.

## Usage

```
brave-assets

Usage: brave-assets [-d|--dump-path PATH] [-r|--refresh]

Available options:
  -d,--dump-path PATH      Path where to dump images. Default is assets/
  -r,--refresh             Redownload all images. Deactivated by default
  -h,--help                Show this help text
```

Just run `brave-assets` and it will start to scrape through the Brave Frontier Global wiki. Do note that this will take some (about an hour) and it's also quite heavy (around 1GB).

## TODO

Some units in the wiki don't have their names 1:1 as Deathmax' dump, most of them being collab units that are named the same in each evolution, so the wiki has a naming scheme such as:
* Rain_(7★)
* Rain_(Omni)

So this is not beig parsed correctly, at the moment.

Alternate arts are also not taken into account.

I will eventually (soon, hopefully) fix this.
