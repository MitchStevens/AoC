module DayLenses where

_year :: Lens DateTime Int
_year = lens year (\dt x -> dt {year = x} )

_month :: Lens DateTime Int
_month = lens month (\dt x -> dt {month = x} )

_day :: Lens DateTime Int
_day = lens day (\dt x -> dt {day = x} )

_hour :: Lens DateTime Int
_hour = lens hour (\dt x -> dt {hour = x} ) 

_minute :: Lens DateTime Int
_minute = lens minute (\dt x -> dt {minute = x} ) 

_second :: Lens DateTime Int
_second = lens second (\dt x -> dt {second = x} ) 