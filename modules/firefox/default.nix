{ inputs, ... }:
{
  flake.modules.homeManager.firefox =
    {
      config,
      osConfig,
      pkgs,

      ...
    }:
    {
      programs.firefox = {
	enable = true;
	  policies = {
	    DisableTelemetry = true;                  # Stop Firefox sending usage data to Mozilla
	    DisableFirefoxStudies = true;             # Opt out of Shield studies
	    DisablePocket = true;                     # Remove Pocket integration
	    DisableFirefoxAccounts = true;            # Disable Firefox Sync (uses Mozilla servers)
	    DisableFormHistory = true;                # Don't save form/search historyxxx
	    DontCheckDefaultBrowser = true;           # Stop nagging about default browser

	    EnableTrackingProtection = {
	      Value = true;
	      Locked = true;
	      Cryptomining = true;                    # Block cryptomining scripts
	      Fingerprinting = true;                  # Block fingerprinting scripts
	    };

	    OfferToSaveLogins = false;                # Don't offer to save passwords (use Bitwarden)
	    PasswordManagerEnabled = false;           # Disable the built-in password manager entirely

	    FirefoxHome = {
	      Pocket = false;                         # Remove Pocket from new tab page
	      Snippets = false;                       # Remove Mozilla snippets/ads from new tab
	      TopSites = false;                       # Remove sponsored top sites
	      Highlights = false;                     # Remove "highlights" section
	      SponsoredTopSites = false;              # Explicitly block sponsored tiles
	    };

	    UserMessaging = {
	      SkipOnboarding = true;                  # Skip the first-run onboarding flow
	      ExtensionRecommendations = false;       # No extension recommendations from Mozilla
	      FeatureRecommendations = false;         # No feature nag popups
	      UrlbarInterventions = false;            # No suggestions in address bar from Mozilla
	      WhatsNew = false;                       # Don't show "what's new" popup after updates
	    };
	  };
	  profiles.${osConfig.host.username} = {
	    isDefault = true;
	    extensions.packages = with inputs.firefox-addons.packages.${pkgs.system}; [
	      bitwarden
	      ublock-origin
	    ];


	    settings = {
	      # -- Telemetry & data collection --
	      "toolkit.telemetry.enabled" = false;
	      "toolkit.telemetry.unified" = false;
	      "toolkit.telemetry.archive.enabled" = false;
	      "datareporting.healthreport.uploadEnabled" = false;  # Stop health report uploads
	      "datareporting.policy.dataSubmissionEnabled" = false;
	      "browser.ping-centre.telemetry" = false;             # Disable ping centre telemetry
	      "browser.newtabpage.activity-stream.telemetry" = false;

	      # -- Tracking & fingerprinting --
	      "privacy.trackingprotection.enabled" = true;
	      "privacy.trackingprotection.socialtracking.enabled" = true;  # Block social media trackers
	      "privacy.fingerprintingProtection" = true;           # RFP lite: resists fingerprinting without breaking sites
	      "privacy.resistFingerprinting" = false;              # Full RFP — very effective but breaks some sites; enable if you don't mind

	      # -- Network & DNS --
	      "network.dns.disablePrefetch" = true;                # Don't prefetch DNS for links on page
	      "network.prefetch-next" = false;                     # Don't prefetch pages Firefox thinks you'll visit
	      "network.http.speculative-parallel-limit" = 0;       # Disable speculative connections
	      "network.predictor.enabled" = false;                 # Disable the network predictor
	      "browser.urlbar.speculativeConnect.enabled" = false; # Don't speculatively connect on urlbar hover

	      # -- Search & urlbar --
	      "browser.search.suggest.enabled" = false;            # No search suggestions (sends keystrokes to search engine)
	      "browser.urlbar.suggest.searches" = false;
	      "browser.urlbar.suggest.quicksuggest.sponsored" = false;   # No sponsored suggestions in urlbar
	      "browser.urlbar.suggest.quicksuggest.nonsponsored" = false; # No Firefox Suggest results

	      # -- New tab page --
	      "browser.newtabpage.activity-stream.showSponsored" = false;
	      "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
	      "browser.newtabpage.activity-stream.feeds.topsites" = false;

	      # -- Cookies & storage --
	      "network.cookie.cookieBehavior" = 1;                 # Block third-party cookies
	      "privacy.firstparty.isolate" = false;                # First-party isolation (breaks some SSO flows; enable for max privacy)

	      # -- HTTPS --
	      "dom.security.https_only_mode" = true;               # HTTPS-only mode everywhere
	      "dom.security.https_only_mode_ever_enabled" = true;

	      # -- WebRTC (IP leaks) --
	      # "media.peerconnection.enabled" = false;              # Disable WebRTC entirely to prevent IP leaks
	      # If you need WebRTC (e.g. video calls), use this instead:
	      "media.peerconnection.ice.default_address_only" = true;  # Limit WebRTC to default interface only

	      # -- Misc privacy --
	      "browser.safebrowsing.malware.enabled" = false;      # Sends URLs to Google; disable if you prefer
	      "browser.safebrowsing.phishing.enabled" = false;     # Same — your call, it does offer real protection
	      "geo.enabled" = false;                               # Disable geolocation API
	      "permissions.default.geo" = 2;                       # Block geo by default for all sites
	      "dom.battery.enabled" = false;                       # Hide battery status from sites (fingerprinting vector)
	      "device.sensors.enabled" = false;                    # Disable sensor APIs (fingerprinting)
	      "browser.formfill.enable" = false;                   # Don't save form data

	      # -- Extensions --
	      "extensions.autoDisableScopes" = 0;                  # Auto-enable extensions installed by Nix
	    };


	    search = {
              force   = true;
              default = "Brave";
              engines = {
		"Brave" = {
		  urls = [{
		    template = "https://search.brave.com/search";
		    params   = [{ name = "q"; value = "{searchTerms}"; }];
		  }];
		  iconUpdateURL  = "https://brave.com/static-assets/images/brave-favicon.png";
		  updateInterval = 24 * 60 * 60 * 1000;
		  definedAliases = [ "@brave" ];
		};
		"Nix Packages" = {
		  urls = [{
		    template = "https://search.nixos.org/packages";
		    params = [
		      { name = "type"; value = "packages"; }
		      { name = "query"; value = "{searchTerms}"; }
		    ];
		  }];

		  icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
		  definedAliases = [ "@np" ];
		};
		"Google".metaData.hidden = true;
		"Bing".metaData.hidden   = true;
              };
	    };
	  };

	profiles.yurii = {
	  isDefault = false;
	  id = 1;
	  search.engines = {
            "Nix Packages" = {
              urls = [{
		template = "https://search.nixos.org/packages";
		params = [
		  { name = "type"; value = "packages"; }
		  { name = "query"; value = "{searchTerms}"; }
		];
              }];

              icon = "${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@np" ];
            };
	  };
	  search.force = true;

	  bookmarks = [
            {
              name = "wikipedia";
              tags = [ "wiki" ];
              keyword = "wiki";
              url = "https://en.wikipedia.org/wiki/Special:Search?search=%s&go=Go";
            }
	  ];

	  settings = {
            "dom.security.https_only_mode" = true;
            "browser.download.panel.shown" = true;
            "identity.fxaccounts.enabled" = false;
            "signon.rememberSignons" = false;
	  };

	  userChrome = ''                         
            /* some css */                        
	  '';                                      

	  # extensions = with inputs.firefox-addons.packages."x86_64-linux"; [
	  #   bitwarden
	  #   ublock-origin
	  #   sponsorblock
	  #   darkreader
	  #   tridactyl
	  #   youtube-shorts-block
	  # ];

	};
      };

    };
}
