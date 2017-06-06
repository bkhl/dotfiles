#!/bin/sh

# TODO: Skip apps that already exist.
# TODO: Check icon extension.
# TODO: Create .desktop files.

set -x
set -e

nativefier --name Deezer 'http://www.deezer.com' --single-instance  ~/Software/Deezer/
nativefier --name Gmail 'https://mail.google.com' --single-instance --internal-urls '^https?://mail.google.com(|/.*)$' ~/Software/Gmail
nativefier --name Hangouts 'https://hangouts.google.com/' --single-instance --internal-urls '^https?://hangouts.google.com(|/.*)$' ~/Software/Hangouts
nativefier --name Messenger --icon ~/Software/Messenger/messenger.svg 'https://www.messenger.com' --single-instance ~/Software/Messenger/
nativefier --name Outlook 'https://mail.dayjob.example.com/owa/' --single-instance --internal-urls '^https?://mail.dayjob.example.com(|/.*)$'  ~/Software/Outlook
nativefier --name Skype 'https://web.skype.com/en/' --single-instance --internal-urls '^https?://web.skype.com(|/.*)$' ~/Software/Skype
nativefier --name Todoist --icon ~/Software/Todoist/todoist.png --single-instance 'https://todoist.com' ~/Software/Todoist/
nativefier --name WhatsApp --icon ~/Software/WhatsApp/whatsapp.png 'https://web.whatsapp.com' --single-instance --internal-urls '^https?://web.whatsapp.com(|/.*)$' ~/Software/WhatsApp/
