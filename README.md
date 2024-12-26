# Chelleport
Control your mouse pointer entirely with your keyboard.

> Note: Current it only supports Linux running X11 display server with a compositor, because that's what I use. Might look into supporting more systems if there is interest.


## Modes
- **Labelled Hints mode (default. `ctrl+t`)**: Displays a grid overlay on your screen, where each cell is labeled with a unique two-key combination. Press the corresponding keys to move the cursor to the desired cell.
- **Text Search mode (`ctrl+s`)**: Uses OCR to identify and highlight words on the screen, allowing you to search for text and move the cursor directly to matching text.


---

https://github.com/user-attachments/assets/93ddc1ff-6cbe-4be4-9507-d68de880212a

---


## Features
- **Search by text**:
  - Use OCR to locate any visible text on the screen and position your cursor precisely.
- **Click**:
  - Press `space` left clicks at current mouse position.
  - Press `shift+space` left clicks and show the grid again.
- **Select text/Drag-n-drop**:
  - Press `ctrl+v` starts dragging/selecting/holding down left mouse button.
  - Press `space` to stop dragging.
  - Press `ctrl+v` again to stop dragging and show the grid again.
- **Double click**:
  - Press `2` followed by `space` will click twice.
  - Any digit key followed by `space` will click that many times.
- **Right click**:
  - Pressing `minus` key right clicks at current mouse position.
  - Holding `shift` key right clicks and shows the grid again.
- **Granular movement**:
  - Once you match with a label on the screen, you can use `hjkl` keys to move your cursor.
  - Holding `shift` key will use bigger steps for movements.
  - You can also repeat movement by pressing a digit before the movement. Eg: `5k` moves 5 small steps up. `5K` moves 5 big steps up.


## Install
- Clone the repo and build it yourself: `cabal build chelleport` or `nix build`
- Nix flakes users can try it out by running: `nix run github:phenax/chelleport#chelleport`


## Usage
Use [sxhkd](https://github.com/baskerville/sxhkd), [shotkey](https://github.com/phenax/shotkey), your window manager or any other key binding manager to set up a keybinding for `chelleport`.

### Hints mode (`ctrl+t` to switch to hints mode)
- With the grid open, type any of the key sequences shown on the grid to move the pointer there
- Once a match is found, you can now use `hjkl` keys to make smaller movements. Hold `shift` + `hjkl` to move in bigger increments.
- Press `space` to click

### Search mode (`ctrl+s` to switch to search mode)
- Words that are recognized by OCR will be highlighted
- Type the characters in one of the words to move the cursor to it
- Press `ctrl+n` & `ctrl+p` to go to next/previous match respectively


## Feedback and Support
Interested in extending platform compatibility or new features? Let me know! Contributions and suggestions are welcome.
