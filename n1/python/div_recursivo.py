def div(n, d):
   if n < d:
      return 0
   return 1 + div(n-d, d)

print(div(8, 2))