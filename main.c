#include <pocketsphinx.h>

#define PPCAT_NX(A, B) A

int main(int argc, char *argv[])
{
    const cmd_ln_t* config = cmd_ln_init(NULL, ps_args(), TRUE,
		         "-hmm", MODELDIR "/en-us/en-us",
	                 "-lm", MODELDIR "/en-us/en-us.lm.bin",
	                 "-dict", MODELDIR "/en-us/cmudict-en-us.dict",
	                 NULL);

    if (config == NULL) {
	fprintf(stderr, "Failed to create config object, see log for details\n");
	return -1;
    }

    const ps_decoder_t* decoder = ps_init(config);
    if (decoder == NULL) {
	fprintf(stderr, "Failed to create recognizer, see log for details\n");
	return -1;
    }

    // If you want to create your own version of it: it needs to be a single-channel (monaural), little-endian, unheadered 16-bit signed PCM audio file sampled at 16000 Hz.
    FILE* audioFile = fopen("shitty-recording.raw", "rb");
    if (audioFile == NULL) {
	fprintf(stderr, "Unable to open input file goforward.raw\n");
	return -1;
    }
    int rv = ps_start_utt(decoder);
    int16 buf[512];

    while (!feof(audioFile)) {
	size_t nsamp;
	nsamp = fread(buf, 2, 512, audioFile);
	rv = ps_process_raw(decoder, buf, nsamp, FALSE, FALSE);
        printf("cur rv: %i - %i \n", rv, nsamp);
    }
    rv = ps_end_utt(decoder);

    
    for(ps_seg_t* iterator = ps_seg_iter(decoder); iterator != NULL; iterator = ps_seg_next(iterator)){
        int begin = 0; int end = 0;
        ps_seg_frames(iterator, &begin, &end);
        char const* word = ps_seg_word(iterator);
        printf("%s (%i, %i) ", word, begin, end);

    }

    int32 score;
    char const* hyp = ps_get_hyp(decoder, &score);
    printf("Recognized: %s\n", hyp);
    printf("score: %i\n", score);

    fclose(audioFile);
    ps_free(decoder);
    cmd_ln_free_r(config);
    printf("outputting results " MODELDIR);
    return 0;
}
