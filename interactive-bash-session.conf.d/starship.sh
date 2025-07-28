if ! command -v starship 2>&1 >/dev/null ; then
    return
fi
# install/update with:`sh -c "$(curl -fsSL https://starship.rs/install.sh)" -- -b ~/.local/bin/ -V`
source <(starship init bash)
