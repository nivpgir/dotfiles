

def servers():
    return [
        ["ty", "server"],
        ["pylsp", "--check-parent-process", "-vvvvvv", "--log-file", "/tmp/piamh-lsp-log.log"],
        ["ruff", "server"],
    ]
