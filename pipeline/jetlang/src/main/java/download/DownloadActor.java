package benchmark.jetlang;

import org.jetlang.channels.Channel;
import org.jetlang.fibers.Fiber;

public class DownloadActor extends Actor {

    public DownloadActor(Channel<String> inChannel,
                         Channel<String> outChannel,
                         Channel<Void> stopChannel,
                         Channel<Void> nextStopChannel,
                         Fiber fiber) {
        super(inChannel, outChannel, stopChannel, nextStopChannel, fiber);
    }

    @Override
    public String act(String payload) {
        return payload.replaceFirst("Requested ", "Downloaded ");
    }
}
