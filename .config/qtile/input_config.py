
from libqtile import qtile
from libqtile.backend.wayland import InputConfig

keyboard_layouts = ["us", "il"]
keyboard_options = ["custom:hyper4", "grp:win_space_toggle"]
kb_config = InputConfig(kb_layout=",".join(keyboard_layouts),
                        kb_options=",".join(keyboard_options))


def input_config():
    '''
    When using the Wayland backend, this can be used to configure input devices.
    '''
    if qtile.core.name == "x11":
        return None
    elif qtile.core.name == "wayland":
        return {
            "type:keyboard": kb_config,
            "type:touchpad": InputConfig(drag=True,
                                         tap=True,
                                         natural_scroll=True,
                                         pointer_accel=0.4),
        }


wl_input_rules = input_config()
