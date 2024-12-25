# Chelleport
Control your mouse pointer with your keyboard

> Note: So far it only supports Linux running X11 display server with a compositor, because that's what I use. Might look into supporting more systems if there is interest.

https://github.com/user-attachments/assets/93ddc1ff-6cbe-4be4-9507-d68de880212a

## Features
- **Text search mode**: Pressing `<c-s>` puts you in search mode which uses OCR to find words on the screen that you can search and move your cursor to.
- **Labelled hints mode**: This is the default mode. It shows a grid on the screen with 2 keys for each cell. You can move to any cell by pressing the keys shown.
- **Click**: Pressing `space` left clicks at current mouse position. Holding `shift` key left clicks and show the grid again.
- **Select text/Drag-n-drop**: Pressing `Ctrl+V` starts dragging/selecting/holding down left mouse button. Press `space` to stop dragging. Or press `Ctrl+V` again to stop dragging and show the grid again.
- **Double click**: Pressing `2` followed by `space` will click twice. Any digit key followed by `space` will click that many times.
- **Right click**: Pressing `minus` key right clicks at current mouse position. Holding `shift` key right clicks and shows the grid again.
- **Granular movement**: Once you match with a label on the screen, you can use `hjkl` keys to move your cursor. Holding `shift` key will use bigger steps for movements. You can also repeat movement by pressing a digit before the movement. Eg: `5k` moves 5 small steps up. `5K` moves 5 big steps up.


## Install
- Clone the repo and build it yourself: `cabal build chelleport`
- Nix flakes users can try it out by running: `nix run github:phenax/chelleport#chelleport`


## Usage
Use [sxhkd](https://github.com/baskerville/sxhkd), [shotkey](https://github.com/phenax/shotkey), your window manager or any other key binding manager to set up a keybinding for `chelleport`.

### Hints mode (default. `<c-h>` to switch to hints mode)
- With the grid open, type any of the key sequences shown on the grid to move the pointer there
- Once a match is found, you can now use `hjkl` keys to make smaller movements. Hold `shift` to move in bigger increments.
- Press `space` to click

### Search mode (`<c-s>` to switch to search mode)
- Words that are recognized by OCR will be highlighted
- Type the characters in one of the words to move the cursor to it
- Press `<c-n>` & `<c-p>` to go to next/previous match respectively

