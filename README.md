# heth-passive

Half of the frontend for the ethereum haskell client, the other half being <a href="https://github.com/Firescar96/heth-active">heth-active</a>

This is a fork of <a href="https://github.com/kjameslubin/hserver-eth">hserver-eth</a>, the Ethereum Blockapps API. Heth-passive exposes an interface for programs to interact with the local ethereum-client-haskell database as well as the ethereum network by forwarding the appropriate requests to the blockapps API.

Heth-active expects this to be running on localhost:30302
