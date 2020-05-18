Scrapbox notification thinner
====

Currently, Scrapbox's slack notification sends a notification when 90 seconds passed after edits, and this can't be configured.
This proxy allows us to change the period to any number of seconds.

```sh
cabal run scrapbox-notify config.yaml
```

config.yaml would look like

```yaml
# Final destination
url: https://discordapp.com/api/webhooks/XXXXXX/slack
# Set the URL to http://example.com:PORT/foobar on Scrapbox
token: foobar
# Notify when the specified number of seconds passed without updates on a page
delay: 900
port: 8057
```