#include "speech_recognition.h"
#include <stdio.h>

void see_if_it_works(detected_words words){
  for(int i = 0; i < words.used; i++){
    word_frame dd = words.frames[i];
    printf("(%i ~ %i = %s) \n", dd.from_frame, dd.to_frame, dd.word);
  }
}

int main(int argc, char *argv[]){
  detect_result* res = detect_words("tmptmp/speechdetect.raw");
  switch(res->code){
    case SUCCESS: 
      see_if_it_works(res->words);
      break;
  default: printf("error %i", res->code);
  }
  return 0;
}

