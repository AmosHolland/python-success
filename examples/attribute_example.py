c = type('', (), {})()
c.x = 3
(lambda x: x.x)(c)