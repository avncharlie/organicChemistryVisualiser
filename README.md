# Organic Chemistry Visualiser

> A tool to visualise organic compounds 

OCV (Organic Chemistry Visualiser) is a project that parses basic IUPAC nomenclature and renders the described compound.

![Rendered image of TNT rendered in Organic Chemistry Visualiser (1-methyl-2,4,6-trinitrobenzene)](https://github.com/avncharlie/organicChemistryVisualiser/blob/master/Documentation/screenshots/tnt.png?raw=true)

## Usage
OCV is a project I wrote to learn about how parsing and lexical analysis works. It should however be used as a visualiser, not as a IUPAC name verifyer. This is as it attempts to be as accurate in parsing but has many limitations (take a look "What OCV can't do" section). 


## Use it
1. Go to the latest release on the [releases page](https://github.com/avncharlie/organicChemistryVisualiser/releases).
2. Download the .exe file for your system. If you're unsure, get the 32 bit version.
3. No installation, double click to run
4. 🧪 🎉

If something isn't working like you think it should, make sure you read through what OCV can and can't do below. If you are still stuck, [create an issue](https://github.com/avncharlie/organicChemistryVisualiser/issues/new).

## What OCV can't do
* Locants cannot be guessed - they have to explicitly stated in every case.
  * e.g. `hexandioic acid` has to be entered as `hexan-1,6-dioic acid`.
  * Some simple cases have been patched - e.g. `ethene` can be entered instead of `eth-1-ene`. Specifically eth/ene/yne and prop/ene/yne names no longer need locants.
* Locants outside a main chain – e.g. `1-hexene` will not parse. `hex-1-ene` should be used instead.
* Nitrogen locants (e.g. `N-methyl`) are not supported.
* Any functional group not supported.
* Take a look at the three "Complex Example" names for examples of complicated nested organic compounds. They give a good overview of OCV's functionality. 
![File menu in OCV showing the examples](https://github.com/avncharlie/organicChemistryVisualiser/blob/master/Documentation/screenshots/examples.png?raw=true)

## What OCV can do
* Straight chain and cyclical main bases
* The following functional groups (attached after the main chain, e.g. `-ol`, or as a substituent before the main chain, e.g. `hydroxy-`)
  * Alkenes and Alkynes
  * Acetyl groups
  * Fluoro, Chloro, Bromo and Iodo groups
  * Amide groups
  * Carboxylic acid
  * Aldehydes
  * Ketones
  * Alcohols
  * Amines
  * Nitro groups
  * Recursive substituents (substituents with substituents)
* Error messages on impossible / unparseable names, e.g `hex-1,2-diyne`
![Image of an error message on the name (hex-1,2-diyne). The error message is: "carbon at locant 2 overloaded with 6 bonds (carbon valency: 4)"]( https://github.com/avncharlie/organicChemistryVisualiser/blob/master/Documentation/screenshots/error.png?raw=true)

Example of a more complex name (not an actual compound but possible):
`2,4,6-trihexyl-1,3,5-tri(7,5-di(5-cyclobutyl10-carboxy-6-nitrodecan-3,8-diynyl)cyclodecanyl)cyclohexane` 
![Rendered image of 2,4,6-trihexyl-1,3,5-tri(7,5-di(5-cyclobutyl10-carboxy-6-nitrodecan-3,8-diynyl)cyclodecanyl)cyclohexane](https://github.com/avncharlie/organicChemistryVisualiser/blob/master/Documentation/screenshots/complexExample.png?raw=true)

## Technical details
If you're interested, take a look at [howOCVworks.txt](https://github.com/avncharlie/organicChemistryVisualiser/blob/master/Documentation/howOCVworks/howOCVworks.txt). It is a very comprehensive and (hopefully!) easy to read guide on how OCV works. Also take a look at the [structure chart](https://github.com/avncharlie/organicChemistryVisualiser/blob/master/Documentation/howOCVworks/Structure%20Chart.pdf) to see a visual overview of OCV (made with [draw.io](https://www.draw.io/)).

![structure chart](https://github.com/avncharlie/organicChemistryVisualiser/blob/master/Documentation/howOCVworks/Structure%20Chart.png?raw=true)


## Feedback
If you have any questions or comments, I'd love to hear from you!
Send me an email on <alvinjoycharles@gmail.com>.

## License
This project is licensed under the MIT License - see the LICENSE file for details


## Acknowledgments
* I referred  to Cambridge University's [OPSIN](https://opsin.ch.cam.ac.uk/index.html) project a lot. See the online interface [here](https://opsin.ch.cam.ac.uk/index.html).
* [jaimebuilds](https://github.com/jamiebuilds)' The Super Tiny Compiler project helped me understand how parsing works. See it [here](https://github.com/jamiebuilds/the-super-tiny-compiler).
* I used "carbon rings" by [Ben Davis](https://thenounproject.com/smashicons/) from the Noun Project as the application logo.