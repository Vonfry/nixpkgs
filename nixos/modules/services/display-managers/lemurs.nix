{config, lib, pkgs, ... }:

let
  dmcfg = config.services.xserver.displayManager;
  cfg = config.services.displayManager.lemurs;

  lemurs = cfg.package;

  tomlFmt = pkgs.formats.toml { };

  inherit (lib)
    mkIf mkOption mkEnableOption mkPackageOption
  ;

  defaultConfig = {
      tty = 2;
      pam_service = "lemurs";
      "power_controls.base_entries" = [
        {
          hint = "Shutdown";
          hint_color = "dark gray";
          hint_modifiers = "";
          key = "F1";
          cmd = "/run/current-system/sw/bin/systemctl poweroff -l";
        }
        {
          hint = "Reboot";
          hint_color = "dark gray";
          hint_modifiers = "";
          key = "F2";
          cmd = "/run/current-system/sw/bin/systemctl reboot -l";
        }
      ];
  };

  finalConfig = lib.recursiveUpdate defaultConfig cfg.settings;

  cfgFile = tomlFmt.generate "config.toml" finalConfig;

  make-x-scripts = path: pkgs.runCommand "session-scripts" {} ''
    mkdir $out
    for session in $(find ${path} -name '*.desktop'); do
      filename=$(basename $session)
      stem=''${filename%.*}
      xsession_cmd="$(grep '^Exec=/nix/store/.*$' $session | cut -c 6-)"
      echo "startx $xsession_cmd" > $out/$stem
      chmod +x $out/$stem
    done
  '';
  copy-scripts = path: pkgs.runCommand "session-scripts" {} ''
    mkdir $out
    for session in $(find ${path} -name '*.desktop'); do
      filename=$(basename $session)
      stem=''${filename%.*}
      cp "$(grep '^Exec=/nix/store/.*$' $session | cut -c 6-)" "$out/$stem"
    done
  '';
  xsessions = make-x-scripts true "${dmcfg.sessionData.desktops}/share/xsessions";
  wayland-sessions = copy-scripts "${dmcfg.sessionData.desktops}/share/wayland-sessions";
in
{
  options = {
    services.displayManager.lemurs = {
      enable = mkEnableOption "lemurs as the display manager";

      package = mkPackageOption pkgs [ "lemurs" ] { };

      settings = mkOption {
        type = tomlFmt.type;
        default = { };
        example = { };
        description = lib.mdDoc ''
          Extra settings merged in and overwriting defaults in config.toml.
        '';
      };
    };
  };

  config = mkIf cfg.enable {

    assertions = [
      {
        assertion = !dmcfg.autoLogin.enable;
        message = ''
             doesn't support auto login.
        '';
      }
    ];

    # FIXME github:coastalwhite/lemurs#166
    security.pam.services.lemurs = {
      startSession = true;
      unixAuth = true;
      setLoginUid = true;
    };

    environment = {
      etc."lemurs/wms".source = xsessions;
      etc."lemurs/wayland".source = wayland-sessions;
      systemPackages = [ lemurs ];
    };

    services = {
      dbus.packages = [ lemurs ];
      xserver = {
        displayManager.startx.enable = lib.mkDefault config.services.xserver.enable;
        displayManager.job.execCmd = "exec /run/current-system/sw/bin/lemurs --config ${cfgFile}";
        # To enable user switching, allow ly to allocate TTYs/displays dynamically.
        tty = null;
        display = null;
      };
    };

    systemd = {
      services.display-manager = {
        after = [
          "systemd-user-sessions.service"
          "plymouth-quit-wait.service"
          "getty@tty${toString finalConfig.tty}.service"
        ];

        conflicts = [
          "getty@tty7.service"
        ];

        serviceConfig = {
          Type = "idle";
          StandardInput = "tty";
          TTYPath = "/dev/tty${toString finalConfig.tty}";
          TTYReset = "yes";
          TTYVHangup = "yes";
        };
      };
    };
  };

  meta.maintainers = with lib.maintainers; [ vonfry ];
}
