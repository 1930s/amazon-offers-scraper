{-# LANGUAGE OverloadedStrings #-}

module Amazon.Scraper.Offers where

import           Control.Monad.Reader        (Reader, runReader, ask, liftIO)
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.ByteString.Lazy.Char8  as BSLC
import           Data.ByteString.Lazy.Char8  (pack, unpack, dropWhile)
import           Extra.Util.Func             (cleanupText, extractCurrency)
import           Data.Maybe                  (fromMaybe)
import           Text.HTML.Scalpel.Core      ((@:), (@=), scrapeStringLike, text, attr, html, htmls)

type PageHTML = BSL.ByteString
type OfferHTML = BSL.ByteString

-- | Contains all scraped data from the Amazon Offers PageHTML.
data AmazonProduct = AmazonProduct { title :: Maybe BSL.ByteString,
                                     image :: Maybe BSL.ByteString,
                                     azASIN :: Maybe BSL.ByteString,
                                     offers :: [AmazonOffer]
                                   } deriving (Show)

-- | Contains all scraped data of a single offer from the Amazon Offers PageHTML.
data AmazonOffer = AmazonOffer { price :: Maybe Float,
                                 shipping :: Maybe Float,
                                 totalPrice :: Maybe Float,
                                 condition :: Maybe BSL.ByteString,
                                 description :: Maybe BSL.ByteString,
                                 sellerName :: Maybe BSL.ByteString,
                                 sellerRating :: Maybe BSL.ByteString
                               } deriving (Show)


-- | Creates an 'AmazonProduct' based off Amazon Offers webpage html.
-- createAmazonProduct :: PageHTML -> AmazonProduct
createAmazonProduct webpage = runReader createProd webpage
  where createProd = do
          title <- scrapeTitle
          img <- scrapeImage
          azASIN <- scrapeASIN
          offers <- scrapeOffers
          return AmazonProduct {  title = title,
                                  image = img,
                                  azASIN = azASIN,
                                  offers = offers
                                }

-- | Scrapes product title from webpage.
scrapeTitle :: Reader PageHTML (Maybe BSL.ByteString)
scrapeTitle = ask >>= return . fmap cleanupText . flip scrapeStringLike (text titleSelector)
  where titleSelector = "h1" @: ["role" @= "main"]

-- | Scrapes image from webpage.
scrapeImage :: Reader PageHTML (Maybe BSL.ByteString)
scrapeImage = ask >>= return . flip scrapeStringLike (attr "src" imageSelector)
  where imageSelector = "img" @: ["alt" @= "Return to product information"]

-- | Scrapes Amazon ASIN from webpage.
scrapeASIN :: Reader PageHTML (Maybe BSL.ByteString)
scrapeASIN = ask >>= return . fmap getASIN . flip scrapeStringLike (html asinSelector)
  where asinSelector = "div" @: ["id" @= "olpProductImage"]
        getASIN = BSLC.takeWhile (/= '/') . BSLC.dropWhile (/= 'B')

-- | Scrapes all 'AmazonOffer's from webpage.
scrapeOffers :: Reader PageHTML [AmazonOffer]
scrapeOffers = ask >>= return . fmap (runReader createAmazonOffer) . fromMaybe [] . flip scrapeStringLike (htmls offersSelector)
  where offersSelector = "div" @: ["class" @= "a-row a-spacing-mini olpOffer"]


-- | Creates an 'AmazonOffer' based off of the offer html.
createAmazonOffer :: Reader PageHTML AmazonOffer
createAmazonOffer = ask >>= return . runReader createOffer
  where createOffer = do
          shipping <- scrapeOfferShipping
          price <- scrapeOfferPrice
          condition <- scrapeOfferCondition
          description <- scrapeOfferDescription
          sellerName <- scrapeOfferSellerName
          sellerRating <- scrapeOfferSellerRating
          return sellerRating
          return $ AmazonOffer {  price = price,
                                  shipping = shipping,
                                  totalPrice = (+) <$> price <*> shipping,
                                  condition = condition,
                                  description = description,
                                  sellerName = sellerName,
                                  sellerRating = sellerRating }


-- | Scrape offer price from offer html.
scrapeOfferPrice :: Reader OfferHTML (Maybe Float)
scrapeOfferPrice = ask >>= return . extractCurrency . fromMaybe "" . flip scrapeStringLike (text priceSelector)
  where priceSelector = "span" @: ["class" @= "a-size-large a-color-price olpOfferPrice a-text-bold"]

-- | Scrape offer shipping from offer html.
scrapeOfferShipping :: Reader OfferHTML (Maybe Float)
scrapeOfferShipping = ask >>= return . extractShipping . fromMaybe " " . flip scrapeStringLike (text shippingSelector)
  where shippingSelector = "p" @: ["class" @= "olpShippingInfo"]
        extractShipping s = if (BSLC.elem 'F' s) then Just 0.00 else extractCurrency s

-- | Scrape offer condition from offer html.
scrapeOfferCondition :: Reader OfferHTML (Maybe BSL.ByteString)
scrapeOfferCondition = ask >>= return . fmap cleanUsedConditionText . flip scrapeStringLike (text conditionSelector)
  where conditionSelector = "span" @: ["class" @= "a-size-medium olpCondition a-text-bold"]
        cleanUsedConditionText = cleanupText . BSLC.drop 2 . BSLC.dropWhile (/= '-')

-- | Scrape offer description from offer html.
scrapeOfferDescription :: Reader OfferHTML (Maybe BSL.ByteString)
scrapeOfferDescription = ask >>= return . fmap cleanupText . flip scrapeStringLike (text descriptionSelector)
  where descriptionSelector = "div" @: ["class" @= "expandedNote"]

-- | Scrape offer seller name from offer html.
scrapeOfferSellerName :: Reader OfferHTML (Maybe BSL.ByteString)
scrapeOfferSellerName = ask >>= return . fmap cleanupText . flip scrapeStringLike (text sellerNameSelector)
  where sellerNameSelector = "h3" @: ["class" @= "a-spacing-none olpSellerName"]

-- | Scrape offer seller rating from offer html.
scrapeOfferSellerRating :: Reader OfferHTML (Maybe BSL.ByteString)
scrapeOfferSellerRating = ask >>= return . fmap cleanupRating . flip scrapeStringLike (text sellerRatingSelector)
  where sellerRatingSelector = "p" @: ["class" @= "a-spacing-small"]
        cleanupRating = cleanupText . BSLC.drop 45
