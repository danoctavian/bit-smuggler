sudo iptables -t nat -I OUTPUT -p tcp -o lo --dport 4001 -j REDIRECT --to-ports 1080
