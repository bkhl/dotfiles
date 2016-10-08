#!/bin/sh

if ping -c 3 8.8.8.8 -q; then
	echo Connection is up
else
	/usr/syno/bin/synovpnc kill_client
	cat <<EOF > /usr/syno/etc/synovpnclient/vpnc_connecting
conf_name=NAME
conf_id=ID
EOF
	/usr/syno/bin/synovpnc reconnect --protocol=openvpn --name=AirVPN
fi
