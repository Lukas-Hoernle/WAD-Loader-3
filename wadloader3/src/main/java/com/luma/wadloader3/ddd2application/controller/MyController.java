package com.luma.wadloader3.ddd2application.controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@CrossOrigin(origins = "https://localhost:3000")
public class MyController {
    @GetMapping("/something")
    public String undocumentedPath() {
        return "Super Secret Stuff lol";
    }
}
