package com.luma.wadloader3.ddd2application.controller;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController("/something")
public class MyController {
    @GetMapping
    public String undocumentedPath() {
        return "Super Secret Stuff lol";
    }
}
