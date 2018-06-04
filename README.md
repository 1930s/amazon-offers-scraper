# Amazon Offers Scraper

This library scrapes available offers off of an Amazon product page offers list. This use to be part of Amazon's API but was deprecated years ago, so this library was born.

Must use amazon offers page, such as: "https://www.amazon.ca/gp/offer-listing/ASIN-HERE/"

Currently only designed to support amazon.ca, but may possibly also work on amazon.com depending on UI changes on Amazon's part.

The entire library is pure and uses Maybe to offer flexibility and refrain from runtime errors if/when Amazon updates their UI.

Data Scraped Includes:
- Product Title
- Product Thumbnail Image URL
- Product ASIN
- Offer Price
- Offer Shipping Price
- Offer Condition
- Offer Description
- Offer Seller Name
- Offer Seller Rating

## How To Use

The primary function to be used is `createAmazonProduct`:

```
createAmazonProduct :: PageHTML -> AmazonProduct
```

Simply call it with the PageHTML which is a ByteString. From there you have access to all of the data.
```
let azProduct = createAmazonProduct webpageHTML
title azProduct
image azProduct
azASIN azProduct
offers azProduct
```

## Documentation

After cloning the repository run the following command to generate documentation:
```
stack haddock amazon-offers-scraper 
```
