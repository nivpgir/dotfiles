# pacstrap /mnt base linux linux-firmware base-devel bash-completion [amd/intel]-ucode networkmanager
# genfstab -U /mnt > /mnt/etc/fstab
# arch-chroot /mnt

sed -ie 's/^# en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen
sed -ie 's/^# en_IL/en_IL/' /etc/locale.gen
locale-gen

export LANG="en_IL.UTF-8"

ln -sf /usr/share/zoneinfo/Asia/Jerusalem /etc/localtime

systemctl enable --now NetworkManager
echo Connect to WiFi with nmtui if necessary

useradd -G wheel -m piamh
echo Choose root password:
passwd
echo Choose piamh password:
passwd piamh

sed -ie 's/^# %wheel ALL=(ALL:ALL) ALL$/%wheel ALL=(ALL:ALL) ALL/' /etc/sudoers

# drivers
pacman -S xf86-video-amdgpu


su piamh
mkdir -p ~/.local/bin/
(
    mkdir build-zone
    cd build-zone
    # AUR helper
    curl --proto '=https' --tlsv1.2 -sS https://sh.rustup.rs | sh
    mkdir -p ~/.local/share/bash-completion/completions/
    rustup completions bash rustup  >> ~/.local/share/bash-completion/completions/rustup
    rustup completions bash cargo  >> ~/.local/share/bash-completion/completions/cargo
    rustup component add rust-src
    (
	git clone https://aur.archlinux.org/paru
	cd paru/
	sed -ie 's/makedepend/# makedepend/g'
	makepkg -si
    )
    (
	git clone https://github.com/elkowar/eww.git
	cd eww
	cargo build --release --no-default-features --features=wayland
	cp .target/release/eww ~/.local/bin/eww
    )
)
# desktop environment
paru -S greetd-tuigreet-bin greetd polkit river ristate kickoff kmonad-bin gtk-layer-shell
sudo sed -ie 's/command =.*/command = "tuigreet --remember --remember-user-session --user-menu -t --user-menu-min-uid 1000"' /etc/greetd/config.toml
systemctl --enable greetd
sudo cp /usr/share/wayland-sessions/river.desktop /usr/share/wayland-sessions/greetd-user-session.desktop
sudo sed -ie 's/Exec=.*/Exec=/home/piamh/.greetd-session' /usr/share/wayland-sessions/greetd-user-session.desktop

# commonly used programs
paru -S openssh emacs-nativecomp wezterm firefox syncthing keepass-xc zoom discordsystemctl --enable --now syncthing jq

# system info
paru -S htop

# cli utils
paru -S ripgrep fd direnv starship unzip

# package management helpers
paru -S paru python-pipx

# audio
paru -S alsa-utils pipewire pipewire-alsa pipewire-pulse wireplumber helvum
systemctl --user enable --now wireplumber@piamh

# bluetooth
paru -S bluez-utils blueman

# tuxedo drivers
paru -S tuxedo-backlight-control-git tuxedo-control-center-bin tuxedo-keyboard-tools tuxedo-touchpad-switch

mkdir -p  ~/.local/share/fonts

curl -L 'https://fonts.google.com/download?family=Noto%20Sans%20Mono' > ~/Downloads/Noto_Sans_Mono.zip
curl -L 'https://fonts.google.com/download?family=Noto%20Sans%20Symbols' > ~/Downloads/Noto_Sans_Symbols.zip
curl -L 'https://fonts.google.com/download?family=Noto%20Sans%20Symbols%202' > ~/Downloads/Noto_Sans_Symbols_2.zip
curl -L 'https://fonts.google.com/download?family=Noto%20Emoji' > ~/Downloads/Noto_Emoji.zip

unzip ~/Downloads/Noto_Sans_Mono.zip -d ~/.local/share/fonts/noto-sans-mono
unzip ~/Downloads/Noto_Sans_Symbols.zip -d ~/.local/share/fonts/noto-sans-symbols
unzip ~/Downloads/Noto_Sans_Symbols_2.zip -d ~/.local/share/fonts/noto-sans-symbols-2
unzip ~/Downloads/Noto_Emoji.zip -d ~/.local/share/fonts/noto-emoji

echo "source ~/.config/.profile.piamh" >> ~/.profile
echo "source ~/.config/.bashrc " >> ~/.bashrc
