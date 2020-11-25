Hi John, yes, you just need to do matrix multiplication.


| xt |     |  aa  ab  ac   |  | x |

| yt |  =  |  ba   bb  bc  |  | y |

| zt |     |  ca   cb  cc  |  | z |


Where 

* `xt` = transformed coord
* `x`  = coord in ptx file 
* `aa` = matrix.


Then you just need to do the trig on (xt,yt,zt) to get the zenith and azimuth. Then see how nicely behaved the missed shots are.


Still wrestling with the memory fault on scan 2. It is subtle.
