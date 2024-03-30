import ./make-test-python.nix ({ lib, ... }:

{
  name = "lemurs";

  nodes.machine = { ... }: {
    imports = [ ./common/user-account.nix ];
    services.displayManager.lemurs = {
      enable = true;
    };
    services.xserver.enable = true;
    services.xserver.displayManager.defaultSession = "none+icewm";
    services.xserver.windowManager.icewm.enable = true;
  };

  testScript = { nodes, ... }: let
    user = nodes.machine.users.users.alice;
  in ''
      start_all()
      machine.wait_until_tty_matches("2", "password:")
      machine.send_key("ctrl-alt-f2")
      machine.sleep(1)
      machine.screenshot("lemurs")
      machine.send_key("tab")
      machine.send_chars("alice")
      machine.send_key("tab")
      machine.send_chars("${user.password}")
      machine.send_key("ret")
      machine.wait_for_file("/home/alice/.config/.Xauthority")
      machine.succeed("xauth merge /home/alice/.config/.Xauthority")
      machine.wait_for_window("^IceWM ")
      machine.screenshot("icewm")
    '';
})
