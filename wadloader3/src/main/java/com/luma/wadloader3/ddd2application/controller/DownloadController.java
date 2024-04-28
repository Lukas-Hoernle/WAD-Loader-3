package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd3domain.files.services.FileZipper;
import com.luma.wadloader3api.api.DownloadApi;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.core.io.Resource;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequiredArgsConstructor
public class DownloadController implements DownloadApi {

    //TODO add implementation of FileZipper
    FileZipper fileZipper;

    @Override
    public ResponseEntity<Resource> downloadWadIdGet(List<Integer> id) {
        return null;
    }

    @Override
    public ResponseEntity<Resource> downloadWadPost(@Valid List<Integer> requestBody) {
        return null;
    }

    @Override
    public ResponseEntity<Resource> downloadWadpackIdGet(Integer id) {
        return null;
    }

    @Override
    public ResponseEntity<Resource> downloadWadpackIdPost(Integer id, @Valid List<Integer> requestBody) {
        return null;
    }
}
