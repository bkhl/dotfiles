#!/bin/sh

# TODO: Skip apps that already exist.
# TODO: Create .desktop files.

set -x
set -e

NATIVEFIER='nativefier --single-instance'

${NATIVEFIER} --name Deezer 'http://www.deezer.com' ~/Software/Deezer/
${NATIVEFIER} --name Gmail --internal-urls '^https?://mail.google.com(/.*)?$' 'https://mail.google.com' ~/Software/Gmail
${NATIVEFIER} --name Hangouts --internal-urls '^https?://hangouts.google.com(/.*)?$' 'https://hangouts.google.com/' ~/Software/Hangouts
${NATIVEFIER} --name Messenger 'https://www.messenger.com' ~/Software/Messenger/
${NATIVEFIER} --name OGS 'https://online-go.com' ~/Software/OGS/
${NATIVEFIER} --name Outlook --internal-urls '^https?://mail.dayjob.example.com(/.*)?$' 'https://mail.dayjob.example.com/owa/' ~/Software/Outlook
${NATIVEFIER} --name Skype --internal-urls '^https?://web.skype.com(/.*)?$' 'https://web.skype.com/en/' ~/Software/Skype
${NATIVEFIER} --name Slack --internal-urls '^https?://(.*\.)?slack.com(/.*)?$' 'https://dayjobapac.slack.com' ~/Software/Slack
${NATIVEFIER} --name Todoist 'https://todoist.com' ~/Software/Todoist/
${NATIVEFIER} --name WhatsApp --internal-urls '^https?://web.whatsapp.com(/.*)?$' 'https://web.whatsapp.com' ~/Software/WhatsApp/
