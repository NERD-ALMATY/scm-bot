# scm-bot
[![Build Status](https://travis-ci.com/NERD-ALMATY/scm-bot.svg?branch=main)](https://travis-ci.com/NERD-ALMATY/scm-bot)

Telegram bot with Scheme interpreter

## Build
- git clone https://github.com/NERD-ALMATY/scm-bot.git
- cd scm-bot
- docker build -t scm-bot .
- docker run -d --name scm-bot -e TELEGA_BOT=YOUR_TOKEN scm-bot
