{ config, pkgs, ... }:
with pkgs.lib; with types; let
  feedType = submodule {
    options = {
      title = mkOption {
        type = nullOr str;
        default = null;
        description = "The name for this feed";
      };
      url = mkOption {
        type = str;
        description = "The URL of the feed";
      };
      toEmail = mkOption {
        type = nullOr str;
        default = null;
        description = "The email address to send feed updates to";
      };
    };
  };
  configOptions = {
    feeds = mkOption {
      type = attrsOf feedType;
      default = {};
      description = "Feeds to subscribe to";
    };
    fromAddress = mkOption {
      type = str;
      description = "The default email address to send from";
    };
    smtpPasswordFile = mkOption {
      type = nullOr str;
      default = null;
      description = "Where the SMTP credentials can be found";
    };
    smtpPassword = mkOption {
      type = nullOr str;
      default = null;
      description = "The SMTP account's password.

You should probably use smtpPasswordFile instead.  I'm not going to tell you
how to live your life, though..";
    };
    smtpServerPort = mkOption {
      type = nullOr port;
      default = null;
      description = "The port the SMTP server runs on";
    };
    smtpServer = mkOption {
      type = str;
      description = "The hostname of the SMTP server";
    };
    smtpUsername = mkOption {
      type = str;
      description = "The SMTP account username";
    };
    toEmail = mkOption {
      type = str;
      description = "The email address to send to";
    };
  };
  configJSON = pkgs.writeText "rss4email-config.json" (builtins.toJSON cfg.config);
  cfg = config.services.rss4email;
in
{
  options = {
    services.rss4email = {
      enable = mkOption {
        type = bool;
        default = false;
        description = "Whether to enable the rss4email service";
      };
      statePath = mkOption {
        type = str;
        default = "/var/lib/rss4email/state.json";
        description = "Where rss4email should store its state";
      };
      user = mkOption {
        type = str;
        default = "rss4email";
        description = "The user to run rss4email as";
      };
      startAt = mkOption {
        type = either str (listOf str);
        default = "daily";
        description = "When to check for new items in feeds";
      };
      protectSMTPPassword = mkOption {
        type = bool;
        default = false;
        description = "If true, the path in config.smtpPassowrd will have ownership assigned to rss4email's user.";
      };
      config = configOptions;
    };
  };

  config = {
    users.extraUsers = mkIf cfg.enable {
      "${cfg.user}" = {
        createHome = false;
        home = "/var/empty";
        isSystemUser = true;
      };
    };

    systemd.services.rss4email = mkIf cfg.enable {
      description = "Check for new items in configured feeds";
      enable = true;
      startAt = cfg.startAt;
      script = "${pkgs.rss4email}/bin/rss4email -C ${configJSON} -S ${cfg.statePath} run";
    };

    systemd.tmpfiles.rules = [
      (mkIf cfg.enable
        "d ${builtins.dirOf cfg.statePath} 0755 ${cfg.user} nogroup - -")
      (mkIf (cfg.enable && ! (isNull cfg.config.smtpPasswordFile) && cfg.protectSMTPPassword)
        "z ${builtins.dirOf cfg.config.smtpPasswordFile} 0700 ${cfg.user} nogroup - -")
    ];
  };
}
