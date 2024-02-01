from tkinter import Misc

def center_wnd(master: Misc, window: Misc):
    screen_width = master.winfo_screenwidth()
    screen_height = master.winfo_screenheight()

    size = tuple(int(_) for _ in window.geometry().split('+')[0].split('x'))
    x = screen_width / 2 - size[0] / 2
    y = screen_height / 2 - size[1] / 2

    window.geometry("+%d+%d" % (x, y))