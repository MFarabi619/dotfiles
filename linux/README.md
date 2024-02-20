# dotfiles
A collection of my dotfile configurations used across different operating systems

[Terminal Setup for Mac](https://www.youtube.com/watch?v=CF1tMjvHDRA&t=46s&ab_channel=JoseanMartinez)

### Install and set up Zsh
```
sudo apt update
sudo apt install zsh -y
```

Install PowerLevel10K Theme for Oh My Zsh
```
git clone https://github.com/romkatv/powerlevel10k.git $ZSH_CUSTOM/themes/powerlevel10k
```

Open `~/.zshrc` and change the the value of `ZSH_THEME`:
```
ZSH_THEME="powerlevel10k/powerlevel10k
```

Source `.zshrc`:
```
source ~/.zshrc
```

Configure PowerLevel10K
```
p10k configure
```

### Install ZSH Plugins
Install zsh-autosuggestions:
```
git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
```

Install zsh-syntax-highlighting:
```
git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
```

Open `~/.zshrc` and modify the plugins line:
```
plugins=(git zsh-autosuggestions zsh-syntax-highlighting web-search)
```

Source zsh again:
```
source ~/.zshrc
```

Install GNU Stow:
```
sudo apt-get install stow
```

Navigate to dotfiles directory and run stow to create symlinks:
```
cd ~/dotfiles
stow .
```
