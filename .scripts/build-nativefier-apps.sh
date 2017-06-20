#!/bin/sh

# TODO: Skip apps that already exist.
# TODO: Create .desktop files.

set -x
set -e

NATIVEFIER='nativefier --single-instance'

nativefier --single-instance --name Deezer 'http://www.deezer.com' ~/Software/Deezer/
nativefier --single-instance --name Gmail --internal-urls '^https?://mail.google.com(/.*)?$' 'https://mail.google.com' ~/Software/Gmail
nativefier --single-instance --name Hangouts --internal-urls '^https?://hangouts.google.com(/.*)?$' 'https://hangouts.google.com/' ~/Software/Hangouts
nativefier --single-instance --name Messenger 'https://www.messenger.com' ~/Software/Messenger/
nativefier --single-instance --name Noisli 'https://www.noisli.com' ~/Software/Noisli/
nativefier --single-instance --name OGS 'https://online-go.com' ~/Software/OGS/
nativefier --single-instance --name Outlook --internal-urls '^https?://mail.dayjob.example.com(/.*)?$' 'https://mail.dayjob.example.com/owa/' ~/Software/Outlook
nativefier --single-instance --name Skype --internal-urls '^https?://web.skype.com(/.*)?$' 'https://web.skype.com/en/' ~/Software/Skype
nativefier --name Slack --internal-urls '^https?://(.*\.)?slack.com(/.*)?$' 'https://dayjobapac.slack.com' ~/Software/Slack
nativefier --single-instance --name Todoist 'https://todoist.com' ~/Software/Todoist/
nativefier --single-instance --name WhatsApp --internal-urls '^https?://web.whatsapp.com(/.*)?$' 'https://web.whatsapp.com' ~/Software/WhatsApp/
