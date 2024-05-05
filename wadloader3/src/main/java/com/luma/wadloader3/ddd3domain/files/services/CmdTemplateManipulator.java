package com.luma.wadloader3.ddd3domain.files.services;

import com.luma.wadloader3.ddd4abstraction.functional.Failable;

import java.nio.file.Path;

public interface CmdTemplateManipulator<T> {
    Failable<Path> fillTemplate(T templateValues);
}
