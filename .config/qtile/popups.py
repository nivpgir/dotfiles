from pathlib import Path
from io import BytesIO

from qtile_extras.popup.toolkit import (
    PopupGridLayout,
    PopupRelativeLayout,
    PopupImage,
    PopupText,
    _PopupLayout
)

from libqtile.images import Img
from gen_keybinding_img import KeyboardPNGFactory, get_kb_map

from libqtile.log_utils import logger

class PopupLayoutDebugger(PopupGridLayout):
    def process_key_press(self, keycode):
        from libqtile.log_utils import logger
        logger.debug(f"keycode: {keycode}")
        super().process_key_press(keycode)

    def process_button_click(self, x, y, button):
        from libqtile.log_utils import logger
        logger.debug(f"x, y, button: {x, y, button}")
        super().process_button_click(x, y, button)

class PopupImageFromMemory(PopupImage):

    def load_image(self):
        img = Img(self.filename.read())
        self.img = img
        if (img.width / img.height) >= (self.width / self.height):
            self.img.scale(width_factor=(self.width / img.width), lock_aspect_ratio=True)
        else:
            self.img.scale(height_factor=(self.height / img.height), lock_aspect_ratio=True)


def show_logs(qtile):


    with Path(logger.handlers[1].baseFilename).open() as log_stream:
        log_lines = "".join(log_stream.readlines()[-40:])

    logs = PopupText(
        text=log_lines,
        can_focus=True,
        row=0,
        col=0,
        # row_span=1,
        # col_span=1,
        foreground="ffff00",
        fontsize="16",
        wrap=True,
        highlight_method="border"
    )

    # qtile.cmd_spawn(f"notify-send {logs_text}")
    screen_info = qtile.select([("screen", None)]).cmd_info()
    layout = PopupGridLayout(
        qtile,
        controls=[logs],
        rows=1,
        cols=1,
        background="000000f0",
        height=screen_info["height"] * 0.9,
        width=screen_info["width"] * 0.8,
        margin=0,
        initial_focus=None,
        highlight_method="border"
    )

    layout.show(centered=True)


def show_keybindings(qtile):

    screen_info = qtile.select([("screen", None)]).cmd_info()
    kb_map = get_kb_map()
    images_bytes = [
        BytesIO(KeyboardPNGFactory(mods, keys).render())
        for mods, keys in kb_map.items()
    ]

    controls = [PopupImageFromMemory(
        filename=images_bytes[0],
        row=0,
        col=0,
        can_focus=True
    )]

    layout = PopupGridLayout(
        qtile,
        rows=1,
        cols=1,
        controls=controls,
        background="00000060",
        height=screen_info["height"] / 4. * 3.,
        width=screen_info["width"] / 4. * 3.,
        margin=0,
        initial_focus=None,
    )

    # controls = [
    #     PopupImage(
    #         filename="~/Pictures/icons/sleep.svg",
    #         pos_x=0.45,
    #         pos_y=0.1,
    #         width=0.1,
    #         height=0.5,
    #         mouse_callbacks={
    #             "Button1": lazy.spawn("/path/to/sleep_cmd")
    #         }
    #     ),
    #     PopupImage(
    #         filename="~/Pictures/icons/shutdown.svg",
    #         pos_x=0.75,
    #         pos_y=0.1,
    #         width=0.1,
    #         height=0.5,
    #         highlight="A00000",
    #         mouse_callbacks={
    #             "Button1": lazy.shutdown()
    #         }
    #     ),
    #     PopupText(
    #         text="Lock",
    #         pos_x=0.1,
    #         pos_y=0.7,
    #         width=0.2,
    #         height=0.2,
    #         h_align="center"
    #     ),
    #     PopupText(
    #         text="Sleep",
    #         pos_x=0.4,
    #         pos_y=0.7,
    #         width=0.2,
    #         height=0.2,
    #         h_align="center"
    #     ),
    #     PopupText(
    #         text="Shutdown",
    #         pos_x=0.7,
    #         pos_y=0.7,
    #         width=0.2,
    #         height=0.2,
    #         h_align="center"
    #     ),
    # ]

    # layout = PopupRelativeLayout(
    #     qtile,
    #     width=1000,
    #     height=200,
    #     controls=controls,
    #     background="00000060",
    #     initial_focus=None,
    # )

    layout.show(centered=True)

if __name__ == "__main__":
    from libqtile.command.client import InteractiveCommandClient
    c = InteractiveCommandClient()
    print(c.commands())

    # show_keybindings(c)
