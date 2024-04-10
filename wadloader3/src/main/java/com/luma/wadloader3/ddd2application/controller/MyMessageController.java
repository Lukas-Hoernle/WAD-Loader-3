package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd3domain.model.MyMessage;
import com.luma.wadloader3.ddd3domain.repository.MyMessageRepo;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class MyMessageController {

    private final MyMessageRepo repo;

    @GetMapping("/messages")
    public List<MyMessage> messages() {
        return repo.findAll();
    }

    @PostMapping("/message")
    public ResponseEntity<MyMessage> message(@RequestBody MyMessage message) throws URISyntaxException {
        MyMessage persistedMessage = repo.save(message);
        return ResponseEntity
                .created(new URI("/api/message/" + message.getId()))
                .body(persistedMessage);
    }

    @GetMapping("/message/{id}")
    public ResponseEntity<?> messageById(@PathVariable int id){
        Optional<MyMessage> message = repo.findById(id);
        return message.map(res -> ResponseEntity.ok().body(res))
                .orElse(new ResponseEntity<>(HttpStatus.NOT_FOUND));
    }
}
