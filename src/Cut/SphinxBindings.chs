module Cut.SphinxBindings where
  
import Control.Monad (liftM, when)
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

#include "speech_recognition.h"

{#enum result_code as ResultCode {upcaseFirstLetter}#}


-- | Splits a video into segments
foreign import ccall "detect_words" detect_words :: CString -> {#type detect_result#}
