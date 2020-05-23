  
{#enum result_code as ResultCode {upcaseFirstLetter}#}


-- | Splits a video into segments
foreign import ccall "detect_words" detect_words :: CString -> {#type detect_result#}
