#!/usr/bin/env python

import sys
import subprocess
import time
import os
from watchdog.observers import Observer
from watchdog.events import FileSystemEventHandler 

class Handler(FileSystemEventHandler):

    def on_modified(self, event):
        if os.path.splitext(event.src_path)[-1].lower() == '.ml':
            subprocess.call(['ocaml', sys.argv[1]])

if __name__ == '__main__':
    while True:
        event_handler = Handler()
        observer = Observer()
        observer.schedule(event_handler, os.path.dirname(sys.argv[1]), recursive=False)
        observer.start()
        try:
            while True:
                time.sleep(1)
        except KeyboardInterrupt:
            observer.stop()
        observer.join()
