module Handler.Query where

import Import
import Data.Aeson
import Blockchain.Data.DataDefs
import Data.ByteString.Lazy as BS



getQueryR :: Handler Html
getQueryR = do
              addHeader "Access-Control-Allow-Origin" "*"
              defaultLayout $  [whamlet|<h1> Queries!

                                        <h3> 
                                        <p>

                                        Resources are located via GET routes. Requests are routed by the return type - blocks, accounts or transactions. We have
                                        a few pre-built simple routes, and support a general querystring syntax.
                                        
                                        Routes are meant to be invoked via XMLHttpRequest and return raw JSON. To see an example of parsed json, look at <a href="/index.html">here</a>.

                                        <h2> Blocks are fetchable by a variety of queries! 
                                        <p>

                                                <ul> 
                                                      <li> Grab the genesis block by <a href="/query/block/number/0">number</a> 
                                                         or by <a href="/query/block/hash/fd4af92a79c7fc2fd8bf0d342f2e832e1d4f485c85b9152d2039e03bc604fdca">hash</a>.  
 
                                                      <li> Find blocks with a fair amount of <a href="/query/block/gasrange/lower/100000/upper/150000">gas usage</a>.

                                                      <li> Or even, get the last 42 blocks - in <a href="/query/block/last/42">real time</a>. 
                                       
                                        <h2> Accounts, too, are easily queried.

                                        <p>

                                              <ul> 
                                                    <li> What's going on at address <a href="/query/account/address/4c712ac73a53046ce491d95f0aebf8ef8cb057">4c712ac73a53046ce491d95f0aebf8ef8cb057</a>? 

                                                    <li> Which accounts have nonce equal to <a href="/query/account/nonce/3">three</a>?
 
                                                    <li> Does anyone have balance  <a href="/query/account/balance/33000000000000000000">33000000000000000000</a>?
 
                                        <h2> See the transations to and from addresses:

                                        <p> 
                                             A popular <a href="/query/transaction/address/1c11aa45c792e202e9ffdc2f12f99d0d209bef70">address</a>.
                               |]
          
