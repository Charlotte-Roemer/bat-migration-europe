---
background: secondary
fields:
  email:
    text: Your email address
fragment: stripe
post_url: https://us-central1-syna-222118.cloudfunctions.net/function-1/charge
prices:
- currency: usd
  text: 20.00$
- currency: usd
  text: 30.00$
- currency: usd
  text: 50.00$
product: Example Product
stripe_token: pk_test_36PckiAlsGm9KmHj9b034GAW
subtitle: Doesn't work in demo
title: Payment Fragment with Multiple Choices
user_input:
  currencies:
  - usd
  - eur
  - cad
  default: "20.00"
weight: 120
---

You can pay for the product by filling this form (provided by Stripe).
