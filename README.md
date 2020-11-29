# scm-bot
Telegram bot with Scheme interpreter

## Build
git clone https://github.com/NERD-ALMATY/scm-bot.git
cd scm-bot
docker build -t scm-bot .
docker run -it --name scm-bot -e TELEGA_BOT=YOUR_TOKEN scm-bot
