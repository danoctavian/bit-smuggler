#!/bin/sh
sudo iptables -t nat -A PREROUTING -p tcp --dport 4000 -j DNAT --to 127.0.0.1:4001
