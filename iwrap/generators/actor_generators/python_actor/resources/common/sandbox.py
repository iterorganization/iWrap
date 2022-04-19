import os
import shutil
from pathlib import Path

from iwrap.common import utils
from .runtime_settings import SandboxMode, SandboxLifeTime


class Sandbox:

    def __init__(self, actor):
        self.actor = actor
        self.sandbox_settings = actor._ActorBaseClass__runtime_settings.sandbox
        self.path = None

    def __set_path(self):
        if self.sandbox_settings.mode == SandboxMode.MANUAL:
            if not self.sandbox_settings.path:
                raise ValueError(f"A path must be set in sandbox MANUAL mode and is empty for actor {self.actor.name}")

            path = Path(self.sandbox_settings.path)

            if not path.exists():
                raise ValueError(f"Actor {self.actor.name}: Sandbox path points to non existing directory ({path})" )

            if not path.is_dir():
                raise ValueError(f"Actor {self.actor.name}: Sandbox path points to a file and not directory ({path})")


            self.path =  self.sandbox_settings.path
        else:
            actor_id = self.actor.unique_id
            sandbox_dir = self.actor.sandbox_default_dir
            sandbox_dir = os.path.expandvars( sandbox_dir )
            sandbox_dir = os.path.expanduser( sandbox_dir )
            sandbox_path = Path( sandbox_dir, actor_id )
            self.path = str(sandbox_path)

    def initialize(self):

        self.__set_path()
        self.create()
        self.clean()


    def create(self):
        Path(self.path).mkdir( parents=True, exist_ok=True)

    def clean(self):
        if self.sandbox_settings.mode == SandboxMode.MANUAL:
            return # It is the user duty to clean sbx in manual mode

        if self.sandbox_settings.life_time == SandboxLifeTime.PERSISTENT:
            return # Sbx content should be kept forever

        sandbox_path = self.path

        for root, dirs, files in os.walk( sandbox_path ):
            for f in files:
                os.unlink( os.path.join( root, f ) )
            for d in dirs:
                if d in ('tmp', ):
                    dirs.remove(d)
                    continue
                shutil.rmtree( os.path.join( root, d ) )

    def remove(self):
        if self.sandbox_settings.mode == SandboxMode.MANUAL:
            return  # It is the user duty to clean sbx in manual mode

        if self.sandbox_settings.life_time == SandboxLifeTime.PERSISTENT:
            return  # Sbx content should be kept forever

        sandbox_path = self.path
        if Path(sandbox_path).exists():
            shutil.rmtree( sandbox_path )
