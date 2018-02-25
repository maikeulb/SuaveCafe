module Errors
open Domain

type Error =
  | TabAlreadyOpened
  | CanNotPlaceEmptyOrder
  | CanNotOrderWithClosedTab
  | OrderAlreadyPlaced
  | CanNotServeNonOrderedDrink of Drink
  | CanNotServeAlreadyServedDrink of Drink
  | CanNotPrepareNonOrderedFood of Food
  | CanNotPrepareAlreadyPreparedFood of Food
  | CanNotServeNonPreparedFood of Food
  | CanNotPrepareNonOrderedFood of Food
  | CanNotPrepareAlreadyPreparedFood of Food
  | CanNotServeNonPreparedFood of Food
  | InvalidPayment of decimal * decimal
  | CanNotPayForNonServedOrder
