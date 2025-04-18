{
  config,
  lib,
  pkgs,
  ...
}:
let
  name = "roon-bridge";
  cfg = config.services.roon-bridge;
in
{
  options = {
    services.roon-bridge = {
      enable = lib.mkEnableOption "Roon Bridge";
      openFirewall = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Open ports in the firewall for the bridge.
        '';
      };
      user = lib.mkOption {
        type = lib.types.str;
        default = "roon-bridge";
        description = ''
          User to run the Roon bridge as.
        '';
      };
      group = lib.mkOption {
        type = lib.types.str;
        default = "roon-bridge";
        description = ''
          Group to run the Roon Bridge as.
        '';
      };
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.roon-bridge = {
      after = [ "network.target" ];
      description = "Roon Bridge";
      wantedBy = [ "multi-user.target" ];

      environment.ROON_DATAROOT = "/var/lib/${name}";

      serviceConfig = {
        ExecStart = "${pkgs.roon-bridge}/bin/RoonBridge";
        LimitNOFILE = 8192;
        User = cfg.user;
        Group = cfg.group;
        StateDirectory = name;
      };
    };

    networking.firewall = lib.mkIf cfg.openFirewall {
      allowedTCPPortRanges = [
        {
          from = 9100;
          to = 9200;
        }
      ];
      allowedUDPPorts = [ 9003 ];
      extraCommands = lib.optionalString (!config.networking.nftables.enable) ''
        iptables -A INPUT -s 224.0.0.0/4 -j ACCEPT
        iptables -A INPUT -d 224.0.0.0/4 -j ACCEPT
        iptables -A INPUT -s 240.0.0.0/5 -j ACCEPT
        iptables -A INPUT -m pkttype --pkt-type multicast -j ACCEPT
        iptables -A INPUT -m pkttype --pkt-type broadcast -j ACCEPT
      '';
      extraInputRules = lib.optionalString config.networking.nftables.enable ''
        ip saddr { 224.0.0.0/4, 240.0.0.0/5 } accept
        ip daddr 224.0.0.0/4 accept
        pkttype { multicast, broadcast } accept
      '';
    };

    users.groups.${cfg.group} = { };
    users.users.${cfg.user} = lib.optionalAttrs (cfg.user == "roon-bridge") {
      isSystemUser = true;
      description = "Roon Bridge user";
      group = cfg.group;
      extraGroups = [ "audio" ];
    };
  };
}
