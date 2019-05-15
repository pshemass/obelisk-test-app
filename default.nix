{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
with import (fetchTarball https://github.com/domenkozar/hie-nix/tarball/master) {};

project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
  shellToolOverrides = self: super: {
      haskell-ide-engine = hie84;
    };
  packages = {
     reflex-dom-echarts = hackGet ./deps/reflex-dom-echarts;
     echarts-jsdom = hackGet ./deps/echarts-jsdom;
     reflex-dom-ace = hackGet ./deps/reflex-dom-ace;
  };
  withHoogle = true;  
})
