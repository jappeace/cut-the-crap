#include <pocketsphinx.h>

#define PPCAT_NX(A, B) A


typedef struct {
    int from_frame;
    int to_frame;
    char* word;
} word_frame;

typedef struct {
  word_frame** frames;
  size_t used;
} detected_words;

typedef struct {} unit;

typedef enum {
                 SUCCESS,               
                 FAILED_CONFIG_OBJECT,
                 FAILED_CREATE_RECOGNIZER,
                 FAILED_UNABLE_INPUTFILE
} result_code ;

typedef struct {
  result_code code;
  union {
    unit u;
    detected_words  words;
  }
} detect_result;

detect_result detect_words(char* filepath){
    const cmd_ln_t* config = cmd_ln_init(NULL, ps_args(), TRUE,
		         "-hmm", MODELDIR "/en-us/en-us",
	                 "-lm", MODELDIR "/en-us/en-us.lm.bin",
	                 "-dict", MODELDIR "/en-us/cmudict-en-us.dict",
	                 NULL);

    detect_result res;
    unit var;
    res.u = var;
    if (config == NULL) {
        res.code = FAILED_CONFIG_OBJECT;
	return res;
    }

    const ps_decoder_t* decoder = ps_init(config);
    if (decoder == NULL) {
        res.code = FAILED_CONFIG_OBJECT;
	return res;
    }

    // If you want to create your own version of it: it needs to be a single-channel (monaural), little-endian, unheadered 16-bit signed PCM audio file sampled at 16000 Hz.
    FILE* audioFile = fopen(filepath, "rb");
    if (audioFile == NULL) {
        res.code = FAILED_UNABLE_INPUTFILE;
	return res;
    }
    int rv = ps_start_utt(decoder);
    int16 buf[512];

    // do the actuall speech recongnition
    while (!feof(audioFile)) {
	size_t nsamp;
	nsamp = fread(buf, 2, 512, audioFile);
	rv = ps_process_raw(decoder, buf, nsamp, FALSE, FALSE);
        printf("cur rv: %i - %i \n", rv, nsamp);
    }
    rv = ps_end_utt(decoder); // make decoder 'realize' recognintion is over

    word_frame** result = (word_frame**) malloc(5*sizeof(word_frame*));
    size_t used = 0;
    size_t size = 5;
    for(ps_seg_t* iterator = ps_seg_iter(decoder); iterator != NULL; iterator = ps_seg_next(iterator)){
        if(used >= size){
          size *= 2;
          result = (word_frame**) realloc(result, size * sizeof(word_frame*));
        }
        word_frame* current_word_frame = (word_frame*) malloc(sizeof(word_frame));

        ps_seg_frames(iterator,
                      &current_word_frame->from_frame,
                      &current_word_frame->to_frame
                      );
        
        char const* word = ps_seg_word(iterator);

        current_word_frame->word = (const char*) malloc(strlen(word)*sizeof(char));
        strcpy(current_word_frame->word, word);

        result[used] = current_word_frame;
        used++;
    }

    fclose(audioFile);
    ps_free(decoder);
    cmd_ln_free_r(config);
    detected_words somevar = {
            result,
            used
    };
    res.code = SUCCESS;
    res.words = somevar;
    return res;
}

void see_if_it_works(detected_words words){
  for(int i = 0; i < words.used; i++){
    word_frame* dd = words.frames[i];
    printf("(%i ~ %i = %s) \n", dd->from_frame, dd->to_frame, dd->word);
  }
}

int main(int argc, char *argv[]){
  detect_result res = detect_words("shitty-recording.raw");
  switch(res.code){
    case SUCCESS: 
      see_if_it_works(res.words);
      break;
  default: printf("error %i", res.code);
  }
  return 0;
}
