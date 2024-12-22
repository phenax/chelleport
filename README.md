# Chelleport
Control your mouse pointer with your keyboard

> Note: Only supports Linux running X11 display server currently because that's what I use. Might support more if there's interest.

https://github.com/user-attachments/assets/93ddc1ff-6cbe-4be4-9507-d68de880212a


## Install
- Clone the repo and build it yourself: `cabal build chelleport`
- Nix flakes users can try it out by running: `nix run github:phenax/chelleport#chelleport`


## Usage
- Use [sxhkd](https://github.com/baskerville/sxhkd), [shotkey](https://github.com/phenax/shotkey), your window manager or any other key binding manager to set up a keybinding for `chelleport`
- With the grid open, type any of the key sequences shown on the grid to move the pointer there
- Once there, you can now use `hjkl` keys to make smaller movements. Hold `shift` to move in bigger increments.
- Press `space` to click


## Features
- **Click**: Pressing `space` left clicks at current mouse position. Holding `shift` key left clicks and show the grid again.
- **Select text/Drag-n-drop**: Pressing `Ctrl+V` starts dragging/selecting/holding down left mouse button. Press `space` to stop dragging. Or press `Ctrl+V` again to stop dragging and show the grid again.
- **Double click**: Pressing `2` followed by `space` will click twice. Any digit key followed by `space` will click that many times.
- **Right click**: Pressing `minus` key right clicks at current mouse position. Holding `shift` key right clicks and shows the grid again.
- **Granular movement**: Once you match with a label on the screen, you can use `hjkl` keys to move your cursor. Holding `shift` key will use bigger steps for movements. You can also repeat movement by pressing a digit before the movement. Eg: `5k` moves 5 small steps up. `5K` moves 5 big steps up.

