package benchmark.jetlang;

import org.jetlang.channels.Channel;
import org.jetlang.fibers.Fiber;

public class WriteActor extends Actor {

    public WriteActor(Channel<String> inChannel,
                      Channel<Void> stopChannel,
                      Fiber fiber) {
        super(inChannel, null, stopChannel, null, fiber);
    }

    @Override
    public String act(String payload) {
        return payload.replaceFirst("Indexed ", "Wrote ");
    }
}