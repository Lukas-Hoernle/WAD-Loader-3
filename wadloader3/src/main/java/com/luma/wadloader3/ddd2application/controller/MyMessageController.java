package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd3domain.model.MyMessage;
import com.luma.wadloader3.ddd3domain.repository.MyMessageRepo;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.security.Principal;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class MyMessageController {

    private final MyMessageRepo messageRepo;

    @GetMapping("/messages")
    public List<MyMessage> messages() {
        return messageRepo.findAll();
    }

    @PostMapping("/message")
    public ResponseEntity<MyMessage> message(Principal principal, @RequestBody MyMessage message) throws URISyntaxException {
        message.setMessage(principal.getName() + " " + message.getMessage());
        MyMessage persistedMessage = messageRepo.save(message);
        return ResponseEntity.created(new URI("/api/message/" + message.getId())).body(persistedMessage);
    }

    @GetMapping("/message/{id}")
    public ResponseEntity<?> messageById(@PathVariable int id) {
        Optional<MyMessage> message = messageRepo.findById(id);
        return message.map(res -> ResponseEntity.ok().body(res))
                .orElse(new ResponseEntity<>(HttpStatus.NOT_FOUND));
    }
}
