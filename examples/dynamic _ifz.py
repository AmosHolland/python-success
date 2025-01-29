num_or_id = lambda x : x if x <= 0 else (lambda y: y)

_ = num_or_id(0)
num_or_id(1)