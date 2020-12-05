# -*- mode: dockerfile; coding: utf-8 -*-
FROM alpine:3.12
RUN apk update
RUN apk add curl git pkgconfig openssl-dev sudo chicken
WORKDIR /root/
RUN chicken-install r7rs
RUN git clone https://github.com/librerush/Telebot
WORKDIR /root/Telebot
RUN chicken-install -s
WORKDIR /root
RUN mkdir scm-bot
COPY . /root/scm-bot
WORKDIR /root/scm-bot
RUN ls -a
RUN chicken-install -test
RUN csc -O3 -R r7rs -o scm-bot-app scm-bot-app.scm
CMD ./scm-bot-app