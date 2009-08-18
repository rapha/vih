import Test.QuickCheck
import EditBuffer

instance Arbitrary Char where
  arbitrary = elements printableRange
    where printableRange = [' '..'~']

prop_insert_delete_inverse ch = 
  emptyBuffer == (deleteChar . moveLeft .insertChar ch) emptyBuffer 

prop_insert_char_advances n = 
  n >= 0 ==> n == x
               where (EditBuffer _ (x,_) contents) = (iterate (insertChar 'a') emptyBuffer) !! n

prop_moveRight_cannot_move_cursor_beyond_last_char n =
  n >= 1 ==>
    (n-1, 0) == loc
      where (EditBuffer _ loc _) = moveRight $ (iterate (insertChar 'a') emptyBuffer) !! n

prop_moveRightOver_can_move_cursor_to_after_last_char n =
  n >= 0 ==>
    (n, 0) == loc
      where (EditBuffer _ loc _) = moveRightOver $ (iterate (insertChar 'a') emptyBuffer) !! n

prop_delete_on_an_empty_line_makes_no_change =
  emptyBuffer == (deleteChar emptyBuffer)
