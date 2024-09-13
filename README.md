# dotfiles
A collection of my dotfile configurations used across different operating systems.

[Stow has forever changed the way I manage my dotfiles](https://www.youtube.com/watch?v=y6XCebnB9gs)
[Arch Linux+Hyprland Install](https://www.youtube.com/watch?v=D8D7voS-G_o&t)

## Requirements

Ensure you have the following installed on your system

### Git

```
pacman -S git
```

### [GNU Stow](https://www.gnu.org/software/stow/)

```
pacman -S stow
```

## Installation

Check out dotfiles repo in $HOME directory using git

```
$ git clone git@github.com/dreamsofautonomy/dotfiles.git
$ cd dotfiles
```

then use GNU stow to create symlinks

``` shell
stow --adopt .
```
