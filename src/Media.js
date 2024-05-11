export const getUserMediaImpl = (navigator) => () => navigator.mediaDevices.getUserMedia({ audio: true })

export const mediaRecorder = (stream) => (ondataavailable) => (onstop) => () => {
    const recorder = new MediaRecorder(stream);
    recorder.ondataavailable = (event) => ondataavailable(event.data)();
    recorder.onstop = onstop;
    return recorder;
}

export const start = (recorder) => () => recorder.start();

export const stop = (recorder) => () => recorder.stop();