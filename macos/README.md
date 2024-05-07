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

## Mouse setup

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/a1a5e18b-1c3b-4708-9be2-4a3dc886a8b9)


## Re-map modifier keys for external keyboard

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/27127400-06bf-49c9-92f7-ae9e2563865b)

## Re-map modifier keys for Apple Internal Keyboard

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/37bab098-baf7-408a-8899-aeb1c54994bc)


## [SurfingKeys](https://chromewebstore.google.com/detail/surfingkeys/gfbliohnnapiefjpjlpjnehglfpaknnc?pli=1) for browser

## [DisplayLink](https://www.synaptics.com/products/displaylink-graphics/downloads/macos) for triple/multi-monitor setup with [StarTech dock](https://www.amazon.ca/Triple-Display-Docking-Station-Universal/dp/B012VKW900/ref=asc_df_B012VKW900/?tag=googleshopc0c-20&linkCode=df0&hvadid=293004044609&hvpos=&hvnetw=g&hvrand=8700524289148619145&hvpone=&hvptwo=&hvqmt=&hvdev=c&hvdvcmdl=&hvlocint=&hvlocphy=9000694&hvtargid=pla-349910448191&mcid=3cd8b0ad72503a5b9e6b12a41cafff96&th=1)

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/be887f39-0dd2-4ebd-aa41-5ca444cf3425)

## [MonitorControl](https://formulae.brew.sh/cask/monitorcontrol) for adjusting brightness across Monitors.

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/04195e2d-8860-4c6d-8e92-c3c3364c6741)
![image](https://github.com/MFarabi619/dotfiles/assets/54924158/b30e6994-61ec-410f-b65a-980dd8c71291)


## [AltTab](https://alt-tab-macos.netlify.app/) to switch between windows

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/937040c0-3129-419d-a61d-9180302437a2)
![image](https://github.com/MFarabi619/dotfiles/assets/54924158/66bc62b4-f6d4-4664-87dd-8a5ef8430727)
![image](https://github.com/MFarabi619/dotfiles/assets/54924158/f0ab830c-c53c-4082-9f1a-0d3c9ab574cf)


## Settings
## Adjust display brightness using keyboard
![image](https://github.com/MFarabi619/dotfiles/assets/54924158/ec6f256d-2cca-4979-95bb-310616400a56)

## Screenshot with cmd+shift+s

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/09ce8880-d11a-45f3-85c7-c4d691f4ec55)

## BetterTouchTool 

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/0710fc72-778e-4cf2-9634-a4b188990441)

#### Switch command+backspace and alt+backspace behaviour

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/5f6fd866-3442-4659-a0c4-0d0fb0cc33cf)

#### Map cmd+tab to ctrl+tab for tab switching on browser

![image](https://github.com/MFarabi619/dotfiles/assets/54924158/f3c9b911-f0ab-4a16-b085-9c53c1800bbd)





