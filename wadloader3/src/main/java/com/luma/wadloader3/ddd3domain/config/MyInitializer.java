package com.luma.wadloader3.ddd3domain.config;

import com.luma.wadloader3.ddd3domain.model.MyMessage;
import com.luma.wadloader3.ddd3domain.repository.MyMessageRepo;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import static java.time.LocalTime.now;

@Component
@RequiredArgsConstructor
public class MyInitializer implements CommandLineRunner {

    private final MyMessageRepo repo;

    @Override
    public void run(String... args) throws Exception {
        repo.save(MyMessage.builder().message(now() + "Somethign").build());
    }
}
